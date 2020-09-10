library(shiny)
library(shinydashboard)
library(plotly)
library(readxl)
library(DT)
library(tidyverse)
library(magrittr)
library(abind)
library(plotly)
library(gridExtra)
library(scales)
library(shinydashboardPlus)
library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
#############################Function List###################################

readplayer <- function(filename){
  data <- read.csv(file=paste0(filename,".csv"), skip=8,header=T)
  data %<>% separate(Timestamp, into = c("Date","TimeStamp","AM/PM"), sep = " ")
  data <- type.convert(data)
  return(data)
}


cleanplayer <- function(data){
  data %<>% separate(Timestamp, into = c("Date","TimeStamp","AM/PM"), sep = " ")
  data <- type.convert(data)
  return(data)
}


#### to app
tofield <- function(playerdata,statium.name,athletes_tb) {
  # coordinates <- read.csv("Field_Coord.csv")
  gamecoords <- subset(coordinates,Stadium==statium.name)
  c3 <- c(gamecoords[gamecoords$Corner==3,5],gamecoords[gamecoords$Corner==3,6])
  c4 <- c(gamecoords[gamecoords$Corner==4,5],gamecoords[gamecoords$Corner==4,6])
  c2 <- c(gamecoords[gamecoords$Corner==2,5],gamecoords[gamecoords$Corner==2,6])
  c1 <- c(gamecoords[gamecoords$Corner==1,5],gamecoords[gamecoords$Corner==1,6])
  v0 <- c3 - c3
  v1 <- c4 - c3
  v2 <- c2 - c3
  basis.1 <- v1/60
  basis.2 <- v2/100
  intermediate <- matrix(c(t(basis.1),t(basis.2)),nrow=2,ncol=2)
  matrix.transform <- solve(intermediate)
  coords <- subset(playerdata, select=c(Longitude,Latitude))
  coords %<>% mutate(Length = Latitude - c3[1],Width = Longitude - c3[2])
  gps.coords <- coords[,c("Length","Width")]
  change <- matrix.transform %*%t(gps.coords)
  field.coords <- as.data.frame(t(change))
  colnames(field.coords) <- c("WidthPosition", "LengthPosition")
  newcoords <- field.coords
  newcoords <- cbind(newcoords, playerdata[, c("Seconds","Player")])
  newcoords <- subset(newcoords, WidthPosition > 0 & WidthPosition<60)
  newcoords <- left_join(newcoords,athletes_tb,by=c("Player" = "First_Name"))
  return(newcoords)
}


############# Read tagged data

videoread <- function(game) {
  inputdirec.tg <- "tag_data"
  filenames <- list.files(inputdirec.tg)
  selectfiles <-filenames[grep("BU", filenames,perl=FALSE)]
  validfilenames <- selectfiles[grep(game,selectfiles,perl=FALSE)]
  selectfiles<-paste0("./",inputdirec.tg,"/",validfilenames)
  videodata <- read.csv(selectfiles,na.strings=c(NA,""))
  return(videodata)
}


## test begin ##
# videoread("Holy Cross 10_26_2019")

# str_replace_all("Holy Cross 10/26/2019", "/", "_")


# test tofield ---> Holy cross game statium= Hart Turf
# tofield(game.test,"Hart Turf")
# summary(tofield(game.test,"Hart Turf"))
#
# newcoords <- tofield(game.test,"Hart Turf")
# subset(newcoords, Seconds >= 500 & Seconds <= 500+1)
#
# makemap(newcoords,500,"Ellie")
# makemap(newcoords,60,"Casey")
## test end ##


############################### Data Preparetion ##############################

## Read gamedata, players data
inputdirec <- "game_data"
filenames<-list.files(inputdirec)
validfilenames <- filenames[grep(".csv",filenames,perl=FALSE)]  ## all csv files name under validfilenames
selectfiles<-paste0("./",inputdirec,"/",validfilenames)

get_pname <- function(a){
  index_name <- a[which(a %in% "for")+1]
  return(index_name)
}

pnames<- unlist(lapply(strsplit(validfilenames," "),FUN = get_pname)) ## get player name 
opponames<- unlist(lapply(strsplit(validfilenames," Export "),"[",1)) ## get school name

nfiles<-length(selectfiles)
resultlist<-vector("list",nfiles)  ## to make a list with length of number of players

for(i in 1:nfiles){
  resultlist[[i]]<- read.csv(selectfiles[i], skip=8,header=T,stringsAsFactors = FALSE)
  resultlist[[i]]$Player <- pnames[i]
  resultlist[[i]]$Opponent <- opponames[i]
}

## ALL Player Data
aggregate <- abind(resultlist,along=1,force.array=F)

## CleanData
aggregate <- cleanplayer(aggregate)

## Add "Gamename & Date"
aggregate$Gamename <- paste(aggregate$Opponent,aggregate$Date)
aggregate$Player <- as.character(aggregate$Player)
## Read Athletes data
athletes <-read_excel("./Athletes_Info/20 Athletes-BU Women Field Hockey.xlsx")
athletes$Date_of_Birth <- as.character(athletes$Date_of_Birth)
## Read Coord Data
coordinates <- read.csv("Field_Coord.csv")
## Read Quarter_Satarttimes data
starttimes <- read.csv("Quarter_Starttime.csv")


# test <- data.frame(Game_name=c("Holy Cross 10/26/2019","Holy Cross 10/26/2019","Holy Cross 10/26/2019","Holy Cross 10/26/2019"),Quarter=c("1","2","3","4"),Starttime=c(29,1278,2989,4247))
# write.csv(test,"Quarter_Starttime.csv",row.names = FALSE)

#####################################-list for input choices-###########################################
game_list <- unique(aggregate$Gamename)
player_list <- unique(aggregate$Player)
stadium_list <- unique(coordinates$Stadium)

#################################################-UI-####################################################

ui <- dashboardPagePlus(
  skin = "red",  ## App skin color
  #######################-SideBar-##################
  dashboardHeaderPlus(title = "BU Field Hockey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "Home", icon = icon("dashboard")),
      menuItem("User Setting Page", tabName = "USP", icon = icon("th")),
      menuItem("Athletes Info Overview", tabName = "AIO", icon = icon("map")),
      menuItem("Performance Overview", tabName = "PO", icon = icon("picture", lib = "glyphicon"),
               menuSubItem("Team Performance", tabName = "TP"),
               menuSubItem("Individual Performance", tabName = "IP")),
      menuItem("Critical Transition Analysis", tabName = "CT", icon = icon("list",lib = "glyphicon")),
      menuItem("Help", tabName = "Help", icon = icon("search",lib = "glyphicon"))
    )#sidebarMenu_end
  ),#dashboardSidebar_end,
  dashboardBody(
    tabItems(
      ################-Home Page-###############
      tabItem(tabName = "Home", 
              #Greetings
              h2("Boston University Women's Field Hockey Performance Portal"),
              
              #Heading image
              fluidRow(h3(""),h3(""),h3(""),column(12,mainPanel(imageOutput("fieldhockey")))),
              
              h4("This Application is developed by Boston University MSSP program. The objective of this app is to help BU Field Hockey Coaches to review athletes' performance both from real games and exercises and to have a better decision making on future coaching.")
      ),#hometabitem_end,
      ################-Upload Game-#############
      tabItem(tabName = "USP", 
              fluidRow(
                navbarPage(title = "User Setting Page", id="navbar1", 
                           tabPanel(title = "Content"),
                           tabPanel(title = "Quit", value="stop", icon = icon("circle-o-notch")))
                ,
                column(4,
                       fileInput('file.game', strong('Upload game data file') ,multiple = TRUE,options(shiny.maxRequestSize = 30*1024^2))
                ),
                column(4,
                       fileInput('file.tag', strong('Upload tag data file') ,multiple = TRUE,options(shiny.maxRequestSize = 30*1024^2))
                       # withBusyIndicatorUI(actionButton("up", "Update"))
                ),
                column(4,
                        fileInput('file.athletes', strong('Upload athletes information file') ,multiple = TRUE,options(shiny.maxRequestSize = 30*1024^2))
                         
                )
              ),
              fluidRow(
                column(4,
                       wellPanel(#style="background: floralwhite",
                                 fluidRow(
                         column(4,  textInput('school', 'School' )),
                         column(4,  textInput('stadium', 'Stadium' )),
                         column(4,  textInput('loc', 'Location') )
                       )),
                       wellPanel(#style="background: floralwhite",
                                 fluidRow(
                         column(6,  textInput('Corlat.1', 'I Latitude' ),
                                textInput('Corlat.2', 'II Latitude' )),
                         column(6,  textInput('Corlon.1', 'I Longitude' ),
                                textInput('Corlon.2', 'II Longitude' ))
                       ),
                       fluidRow(
                         column(6,  textInput('Corlat.3', 'III Latitude' ),
                                textInput('Corlat.4', 'IV Latitude' )),
                         column(6,  textInput('Corlon.3', 'III Longitude' ),
                                textInput('Corlon.4', 'IV Longitude' ),actionButton("do", "Save"))
                       ))),
                column(8,wellPanel(#style="background: floralwhite",
                                   style = "height:500px",
                                   DT::dataTableOutput('table'))
                )
              ) ,fluidRow(
                column(4,wellPanel(#style="background: floralwhite",
                                    style =  "height:450px", 
                                   textInput('game_N', 'Game :' ),
                                    textInput('Q1', 'Quarter I Start Time:' ),
                                    textInput('Q2', 'Quarter II Start Time:' ),
                                    textInput('Q3', 'Quarter III Start Time:' ),
                                    textInput('Q4', 'Quarter IV Start Time:' ),actionButton("do.1", "Save"))),
                column(8,
                       wellPanel(#style="background: floralwhite",
                         style = "height:450px",
                         DT::dataTableOutput('table.q'))
                )
              )     
      )#USP_end
      ,
      ################-User Setting Page-#############
      tabItem(tabName = "AIO",
              fluidRow(navbarPage(title = "Athletes Info Overview", id="navbar2", 
                                  tabPanel(title = "Content"),
                                  tabPanel(title = "Quit", value="stop", icon = icon("circle-o-notch"))),
                       column(width = 12, wellPanel(style="background: floralwhite",style = "height:800px",align="center",
                                                    h3(strong("BU Women Field Hockey Athletes")),
                                                    DT::dataTableOutput("tableAIO")))
              )#fluidRrow_end
      ),#AIOtabitem_end,
      ################Over View Performance - Team Performance ###############
      tabItem(tabName = "TP",
              fluidRow(navbarPage(title = "Team Performance", id="navbar3", 
                                  tabPanel(title = "Content"),
                                  tabPanel(title = "Quit", value="stop", icon = icon("circle-o-notch"))),
                       column(width = 3, 
                              wellPanel(#style="background: floralwhite",
                                        title = "Game",
                                        br(),
                                        selectInput("GameTP", "Select Game:", game_list),width = NULL)),
                       
                       column(width = 9,
                              wellPanel(#style="background: floralwhite",
                                        h3(strong("Team Players Stats")), align="center",style = "height:800px",
                                        hr(),
                                        DT::dataTableOutput("tableTP"), width = 9, height = 500))
              )#fluidRrow_end
      ),#TPtabitem_end,
      
      ################-OverView Performance - INDIVIDUAL Performance-###############
      tabItem(tabName = "IP",
              fluidRow(navbarPage(title = "Individual Performance", id="navbar4", 
                                  tabPanel(title = "Content"),
                                  tabPanel(title = "Quit", value="stop", icon = icon("circle-o-notch"))),
                       column(3,
                              wellPanel(#style="background: floralwhite",
                                        br(),
                                        selectInput("GameIP", "Select Game:", game_list,width = NULL),
                                        hr(),
                                        selectInput("PlayerIP", "Select Player:", player_list,width = NULL),
                                        hr(),
                                        # sliderInput("QuarterIP", "Select Quarter:", min = 1, max = 4, value = 1),
                                        # hr(),
                                        # br(),
                                        h5(strong("Enter Time Range:")),
                                        textInput("start.T","Start Time:"),
                                        textInput("end.T","End Time:"),
                                        #sliderInput("TimerangeIP", "Time Range:", min = 0, max = 900, value = c(200,600)),
                                        #br(),
                                        selectInput("ParametersIP", "Select Parameters:", c("Player.Load"
                                                                                            ,"Acceleration"
                                                                                            ,"Velocity"),width = NULL),
                                        hr(),
                                        actionButton("do.Plot", "Plot")
                              )
                       ),
                       
                       
                       column(9,
                              wellPanel(#style="background: floralwhite", 
                                        align="center",
                                         #fluidRow(textInput("start.T","Start Time"),textInput("end.T","End Time"),actionButton("do.Plot", "Plot")),
                                         #sliderInput("TimerangeIP", h4(strong("Time Range:")), min = 0, max = 900, value = c(200,600)),
                                         #hr(),
                                         h4(strong("Velocity Percentage Change")),
                                         addSpinner(plotlyOutput("Percentage_change_IP",width = "800px",height = "340px"),spin = "rotating-plane", color = "#922B21"),
                                         br(),
                                         h4(strong("Individual Performance")),
                                         addSpinner(plotlyOutput("Individual_Performance",width = "800px",height = "340px"),spin = "rotating-plane", color = "#922B21")
                              )
                       )
                       
              )  ## fluidRow end
              
              
      ),#IPtabitem_end,
      ################-Critical Transition Analysis-#############
      
      
      
      
      
      tabItem(tabName = "CT",
              
              fluidRow( navbarPage(title = "Critical Transition Analysis", id="navbar5", 
                                   tabPanel(title = "Content"),
                                   tabPanel(title = "Quit", value="stop", icon = icon("circle-o-notch"))),
                        column(9,
                               wellPanel(style= "height:225px" ,
                                 #style = "height:225px",
                                 fluidRow(
                                   #select game
                                   column(4, 
                                          selectInput("select.stadium", "Select Stadium:", choices = stadium_list)),
                                   column(4,
                                          selectInput("select.game", "Select Game:", choices =  game_list)),
                                   column(4,
                                          sliderInput("select.quarter", "Select Quarter:", min = 1, max = 4, value = 1)),
                                   column(12,
                                          sliderInput("select.ctnum", "Select #th CT Point:", min = 1, max = 60, value = 1)))
                               )),
                        
                        column(3,
                               wellPanel(style="height:225px",
                                 #style = "height:225px",
                                 selectInput("select.player", "Select Player:", player_list),
                                 br(),
                                 selectInput("select.parameters", "Select Parameters:", c("Player.Load"
                                                                                          ,"Acceleration"
                                                                                          ,"Velocity")))
                               
                        )), # fluid row end
              
              #select Turnover
              fluidRow(
                
                column(6,
                       wellPanel(style="height:950px",
                                 align="center",
                                 #style = "height:950px",
                                 h4(strong("All Player Position")),
                                 # br(),
                                 addSpinner(plotlyOutput("qiuchang",width = "540px"),spin = "rotating-plane", color = "#922B21"))),
                
                
                column(6,
                       wellPanel(style="height:950px",
                                 align="center",
                                 #style = "height:950px",
                                 # h4(strong("Player Performance Plot")),
                                 h4(strong("Velocity Percentage Change")),
                                 addSpinner(plotlyOutput("Percentage_change",width = "540px"),spin = "rotating-plane", color = "#922B21"),
                                 h4(strong("Player Performance")),
                                 addSpinner(plotlyOutput("Player_performance",width = "540px"),spin = "rotating-plane", color = "#922B21")))
              )
              
              
      ),#CTtabitem_end
      
      tabItem(tabName = "Help",
              fluidRow(
                h2("Boston University Women's Field Hockey Performance Portal")
              ),
              br(),
              br(),
              fluidRow(infoBox("Contact Information","Heather Guo heguo5@bu.edu",icon = icon("thumbs-up"),color = "red")
                       ,
                       infoBox("Contact Information","Laura Wang lauraww@bu.edu",icon = icon("thumbs-up"),color = "red")
                       
              ),
              fluidRow(
                h3(""),h3(""),h3(""),column(12,h4("This Application is developed by Boston University MSSP program. Instructor: Prof. Masanao Yajima" )
                )
              )
      )#Help_item_end
      
      
    ) #tabItems_end
  ) #dashboardBody_end
) #dashboardPage_end


#################################################-SERVER-####################################################
server <- function(input,output,session){
  ##########home page###########
  output$fieldhockey<- renderImage({Leg<-"www/fieldhockey.jpg"
  list(src=Leg)
  },deleteFile = FALSE)
  
  output$field.index <- renderImage({Leg<-"www/field.cord.jpg"
  list(src=Leg)
  },deleteFile = FALSE)
  
  ##########Upload Page########### 
  observe({
    if (input$navbar1 == "stop") 
      stopApp()
  })
  
  observe({
    if (input$navbar2 == "stop") 
      stopApp()
  })
  
  observe({
    if (input$navbar3 == "stop") 
      stopApp()
  })
  
  observe({
    if (input$navbar4 == "stop") 
      stopApp()
  })
  
  observe({
    if (input$navbar5 == "stop") 
      stopApp()
  })
  
  observe({  
    if (is.null(input$file.game) ) {    return(NULL)  }  
    file.copy(from = input$file.game$datapath, to =  paste0('game_data/',input$file.game$name )) 
  })
  observe({  
    if (is.null(input$file.tag) ) {    return(NULL)  }  
    
    file.copy(from = input$file.tag$datapath, to =  paste0('tag_data/',input$file.tag$name )) 
  })
  observe({  
    if (is.null(input$file.athletes) ) {    return(NULL)  }  
    
    file.copy(from = input$file.athletes$datapath, to =  paste0('Athletes_Info/',input$file.athletes$name )) 
  })
  
  observeEvent(input$do, {
    Sch<-as.character (input$school)
    Sta<-as.character (input$stadium)
    loc<-as.character (input$loc)
    Corlat.1<-as.numeric(input$Corlat.1)
    Corlat.2<-as.numeric(input$Corlat.2)
    Corlat.3<-as.numeric(input$Corlat.3)
    Corlat.4<-as.numeric(input$Corlat.4)
    Corlon.1<-as.numeric(input$Corlon.1)
    Corlon.2<-as.numeric(input$Corlon.2)
    Corlon.3<-as.numeric(input$Corlon.3)
    Corlon.4<-as.numeric(input$Corlon.4)
    School<-rep(Sch,4)
    Stadium<-rep(Sta,4)
    Location<-rep(loc,4)
    Corner<-c(1,2,3,4)
    Corner<-as.integer(Corner)
    Latitude<-c(Corlat.1,Corlat.2,Corlat.3,Corlat.4)
    Longitude<-c(Corlon.1,Corlon.2,Corlon.3,Corlon.4)
    user_data <- reactive({
      data.frame(School,Stadium,Location,Corner,Latitude,Longitude)
    })
    file.append<-data.frame(School,Stadium,Location,Corner,Latitude,Longitude)
    coordinates<-rbind(coordinates,user_data())
    output$table <- DT::renderDataTable({
      
      DT::datatable(coordinates,options =list(searching = TRUE, pageLength = 40, scrollX = T,scrollY = "350px" ),rownames= FALSE)
    
    })
    
    write.csv(coordinates,"Field_Coord.csv",row.names = FALSE)
    
  })
  
  output$table <- DT::renderDataTable({
    
    DT::datatable(coordinates,options =list(searching = TRUE, pageLength = 40, scrollX = T,scrollY = "350px" ),rownames= FALSE)
  })
  
  ################################quarter
  observeEvent(input$do.1, {
    gam<-as.character (input$game_N)
    q1<-as.numeric(input$Q1)
    q2<-as.numeric(input$Q2)
    q3<-as.numeric(input$Q3)
    q4<-as.numeric(input$Q4)
    Game_name<-rep(gam,4)
    Quarter<-c("1","2","3","4")
    Starttime<-c(q1,q2,q3,q4)
    new<-data.frame(Game_name,Quarter,Starttime)
    
    starttimes<-rbind(starttimes,new)
    output$table.q <- DT::renderDataTable({
      
      DT::datatable(starttimes, options =list(searching = TRUE, pageLength = 40, scrollX = T,scrollY = "300px" ),rownames= FALSE)
      
    })
    
    write.csv(starttimes,"Quarter_Starttime.csv",row.names = FALSE)
    
  })
  output$table.q <- DT::renderDataTable({
    
    DT::datatable(starttimes, options =list(searching = TRUE, pageLength = 40, scrollX = T,scrollY = "300px" ),rownames= FALSE)
    
    })

 
  ########### IP_INDIVIDUAL PERFORMANCE ###########
  
  ########### IP_PERCENTAGE CHANGE PLOT ###########
  
  getIP_Plot1 <- eventReactive(input$do.Plot,{
    ## select game
    filter_game_data <- aggregate[aggregate$Gamename == input$GameIP,]
    
    ## select player
    filter_player_data <- filter_game_data[filter_game_data$Player == input$PlayerIP,]
    
    Veloc_max <- athletes$Max_Vel[athletes$First_Name == input$PlayerIP]
    
    filter_player_data["Veloc_Perc"] <- filter_player_data$Velocity / Veloc_max * 100
    
    
    ## select quarter
    starttimes_t <- starttimes[starttimes$Game_name==input$GameIP,]
    ## separate quarter
    quartersep <- vector("list",4)
    for (i in 1:4){
      if (i < 4){
        quartersep[[i]] <- filter_player_data[(filter_player_data$Seconds>=starttimes_t[i,3]
                                               &filter_player_data$Seconds<(starttimes_t[i+1,3]-120)),]
      }
      else {
        quartersep[[i]] <- filter_player_data[filter_player_data$Seconds>=starttimes_t[i,3],]
      }
    }
    
    # filter_quarter_data <- quartersep[[input$QuarterIP]]
    
    timein <- as.numeric(input$start.T)
    timeout <- as.numeric(input$end.T)
    
    
    filter_player_data_time <-   filter_player_data %>% filter(Seconds > (timein-2) & Seconds < ( timeout +2) )
    
    
    # timein <- input$TimerangeIP[1]
    # timeout <- input$TimerangeIP[2]
    
    pcplot_IP <- ggplot(filter_player_data_time)+geom_point(mapping = aes(x=Seconds,y=Veloc_Perc),
                                                            color="blue")+ labs(x="Seconds",y="Max Velocity Percentage")+
      geom_vline(xintercept = timein, color = '#53535B', linetype = 'dashed') +
      geom_vline(xintercept = timeout, color = '#53535B', linetype = 'dashed') +
      geom_hline(yintercept = 100, color = '#57D047', linetype = 'dashed') +
      geom_hline(yintercept = 50, color = '#ED7461', linetype = 'dashed') + theme_light()
    
    ggplotly(pcplot_IP)
    
  })
  
  output$Percentage_change_IP <- renderPlotly({
    getIP_Plot1()
  })
  
  ########### IP_INDIVIDUAL PERFORMANCE PLOT ###########
  
  getIP_Plot2 <- eventReactive(input$do.Plot,{
  
    ## select game
    filter_game_data <- aggregate[aggregate$Gamename == input$GameIP,]
    ## select player
    filter_player_data <- filter_game_data[filter_game_data$Player == input$PlayerIP,]
    
    Veloc_max <- athletes$Max_Vel[athletes$First_Name == input$PlayerIP]
    
    filter_player_data["Veloc_Perc"] <- filter_player_data$Velocity / Veloc_max * 100
    
    
    ## select quarter
    starttimes_t <- starttimes[starttimes$Game_name==input$GameIP,]
    
    ## separate quarter
    quartersep <- vector("list",4)
    for (i in 1:4){
      if (i < 4){
        quartersep[[i]] <- filter_player_data[(filter_player_data$Seconds>=starttimes_t[i,3]
                                               &filter_player_data$Seconds<(starttimes_t[i+1,3]-120)),]
      }
      else {
        quartersep[[i]] <- filter_player_data[filter_player_data$Seconds>=starttimes_t[i,3],]
      }
    }
    
    # filter_quarter_data <- quartersep[[input$QuarterIP]]
    
    #timein <- input$TimerangeIP[1] #+starttimes[input$QuarterIP,2]
    #timeout <- input$TimerangeIP[2] #+starttimes[input$QuarterIP,2]
    
    timein <- as.numeric(input$start.T)
    timeout <- as.numeric(input$end.T)
    
    
    filter_player_data_time <-  filter_player_data %>% filter(Seconds > (timein-2) & Seconds < ( timeout +2) )
    
    
    plot_IP<-ggplot(filter_player_data_time)+geom_point(mapping = aes_string(x='Seconds',y= input$ParametersIP),color="red")+
      labs(x="Seconds",y= paste0(input$ParametersIP)) +
      #annotate("text",x=max(filter_player_data_time$Seconds)-5,y=Veloc_max,label=Reaction_time)+
      #annotate("text",x=max(filter_player_data_time$Seconds)-5,y=max(filter_player_data_time$Velocity)-1,label=RateofChange)+
      geom_vline(xintercept = timein, color = '#53535B', linetype = 'dashed') +
      geom_vline(xintercept = timeout, color = '#53535B', linetype = 'dashed') + theme_light()
    
    
    ggplotly(plot_IP)
  })
  
  
  
  
    output$Individual_Performance <- renderPlotly({
      getIP_Plot2()
    
  })
  

  
  
  ########### CT_QIUCHANG PLOT ###########
  
  
 observe({ 
   
   #starttimes <- data.frame(quarter=c("Q1","Q2","Q3","Q4"),time=c(29,1278,2989,4247))
   starttimes_t <- starttimes[starttimes$Game_name==input$GameIP,]
   game <- str_replace_all(input$select.game, "/", "_")
   
   video <- videoread(game)
   quarters <- which(!is.na(video$internal.comment)) #Looks at the filled rows marking quarter starts
   quarters <- c(quarters,nrow(video)+1)
   colnames(video)[17] <- "player"
   colnames(video)[12] <- "CT_Type"
   video <- left_join(video,athletes,by=c("player" = "Player_number")) %>% arrange(timeIn)
   
   videosep <- vector("list",4)
   for (i in 1:4){
     videosep[[i]] <- video[quarters[i]:(quarters[i+1]-1),]
     starttime <- videosep[[i]]$timeIn[1]
     videosep[[i]] %<>% mutate(adj.timeIn=timeIn-starttime+starttimes_t[i,3],adj.timeOut=timeOut-starttime+starttimes_t[i,3])
   }
   
   updateSliderInput(session,'select.ctnum', label = 'Select #th CT Point:', min =1 , max = nrow(videosep[[input$select.quarter]]))
   
 })
 
  output$qiuchang <- renderPlotly({
    
    starttimes_t <- starttimes[starttimes$Game_name==input$select.game,]
    game <- str_replace_all(input$select.game, "/", "_")

    video <- videoread(game)
    quarters <- which(!is.na(video$internal.comment)) #Looks at the filled rows marking quarter starts
    quarters <- c(quarters,nrow(video)+1)
    colnames(video)[17] <- "player"
    colnames(video)[12] <- "CT_Type"
    video <- left_join(video,athletes,by=c("player" = "Player_number")) %>% arrange(timeIn)

    videosep <- vector("list",4)
    for (i in 1:4){
      videosep[[i]] <- video[quarters[i]:(quarters[i+1]-1),]
      starttime <- videosep[[i]]$timeIn[1]
      videosep[[i]] %<>% mutate(adj.timeIn=timeIn-starttime+starttimes_t[i,3],adj.timeOut=timeOut-starttime+starttimes_t[i,3])
    }


    filter_game_data <- aggregate[aggregate$Gamename == input$select.game,]

    data <- tofield(filter_game_data,input$select.stadium,athletes)
    # 
    # # starttime <- videosep[[quarter]]$adj.timeOut[turnover]
    starttime <- videosep[[input$select.quarter]]$adj.timeOut[input$select.ctnum]
    pwball <- as.character(videosep[[input$select.quarter]]$First_Name[input$select.ctnum])
    newcoords <- subset(data, Seconds >= starttime & Seconds <= starttime+2)
    lastpoint <- newcoords %>%
      group_by(Player) %>%
      slice(c(n())) %>%
      ungroup()

    balllost <- lastpoint %>%
      subset(Player==pwball)
    
    graphplayers <- ggplot(newcoords) + 
      #coord_fixed(ratio=1, xlim=NULL,ylim=c(0,100)) + 
      coord_fixed(ratio=1, xlim=c(0,60),ylim=c(0,100)) + 
      theme_light() + 
      theme(panel.grid.minor=element_blank(), legend.position="none") + 
      geom_rect(aes(xmin = 0, xmax = 60, ymin = 0, ymax = 100), fill = "palegreen") +
      geom_rect(aes(xmin = 28, xmax = 32, ymin = 100, ymax = 102), fill = "gray") + 
      geom_rect(aes(xmin = 28, xmax = 32, ymin = 0, ymax = -2), fill = "gray") +
      geom_segment(aes(x=20,xend=20,y=0,yend=100), color="black", size = 0.1) + 
      geom_segment(aes(x=40,xend=40,y=0,yend=100), color="black", size = 0.1) +
      geom_segment(aes(x=0,xend=60, y=25,yend=25), color="black", size = 0.1) +
      geom_segment(aes(x=0,xend=60, y=50,yend=50), color="black", size = 0.1) +
      geom_segment(aes(x=0,xend=60, y=75,yend=75), color="black", size = 0.1) + 
      geom_segment(aes(x=0,xend=60, y=100,yend=100), color="black") + 
      geom_segment(aes(x=0,xend=60,y=0,yend=0), color="black") +
      geom_segment(aes(x=0,xend=0,y=0,yend=100), color="black") + 
      geom_segment(aes(x=60,xend=60,y=0,yend=100), color="black") + 
      geom_curve(aes(x=14,xend=46, y=100,yend=100),curvature=1, color="black") +
      geom_curve(aes(x=14,xend=46, y=0,yend=0),curvature=-1, color="black") + 
      geom_curve(aes(x=9,xend=51, y=100,yend=100),curvature=1, color="black", linetype="dashed") + 
      geom_curve(aes(x=9,xend=51, y=0,yend=0),curvature=-1, color="black", linetype="dashed") +
      geom_point(aes(x = WidthPosition, y = LengthPosition,alpha=Seconds,color=Player),size=2) + #Trail before final point
      geom_point(data=lastpoint, aes(x=WidthPosition,y=LengthPosition,color=Player),size=4) + #Final Point
      #geom_point(data=lastpoint, aes(x = WidthPosition, y = LengthPosition), shape=1, size = 4,color="white") + #Circle over player
      geom_text(data=lastpoint, aes(x=WidthPosition,y=LengthPosition,label=Player_number),color="white",size=3) + #Text
      geom_point(data=balllost, aes(x = WidthPosition, y = LengthPosition), shape=1, size = 6) + #Circle over player who lost ball
      labs(x="", y="", title = paste0("The Type of Critical Transition Point is"," ",videosep[[input$select.quarter]]$CT_Type[input$select.ctnum]))  
  
    ggplotly(graphplayers,height = 810)
    
  })
  
  
  
  ########### CT_PLAYER PERFORMANCE PLOT ###########
  output$Player_performance <- renderPlotly({
    
    #starttimes <- data.frame(quarter=c("Q1","Q2","Q3","Q4"),time=c(29,1278,2989,4247))
    starttimes_t <- starttimes[starttimes$Game_name==input$select.game,]
    game <- str_replace_all(input$select.game, "/", "_")
    
    video <- videoread(game)
    quarters <- which(!is.na(video$internal.comment)) #Looks at the filled rows marking quarter starts
    quarters <- c(quarters,nrow(video)+1)
    colnames(video)[17] <- "player"
    #colnames(video)[12] <- "CT_Type"
    video <- left_join(video,athletes,by=c("player" = "Player_number")) %>% arrange(timeIn)
    
    videosep <- vector("list",4)
    for (i in 1:4){
      videosep[[i]] <- video[quarters[i]:(quarters[i+1]-1),]
      starttime <- videosep[[i]]$timeIn[1]
      videosep[[i]] %<>% mutate(adj.timeIn=timeIn-starttime+starttimes_t[i,3],adj.timeOut=timeOut-starttime+starttimes_t[i,3])
    }
    
    
    
    filter_game_data <- aggregate[aggregate$Gamename == input$select.game,]
    filter_player_data <- filter_game_data[filter_game_data$Player == input$select.player,]
    Veloc_max <- athletes$Max_Vel[athletes$First_Name == input$select.player]
    filter_player_data["Veloc_Perc"] <- filter_player_data$Velocity / Veloc_max * 100
    
    
    timein <- videosep[[input$select.quarter]]$adj.timeIn[input$select.ctnum]
    timeout <- videosep[[input$select.quarter]]$adj.timeOut[input$select.ctnum]
    
    
    filter_player_data <-  filter_player_data %>% filter(Seconds > (timein-2) & Seconds < ( timeout +2) )
    
    
    
    react_time <- function(data){
      # Veloc_max <- athletes$Max_Vel[athletes$First_Name == input$select.player]
      loc <- which(data$Velocity == max(data$Velocity))
      data <- data[1:loc, ]
      max <- max(data$Seconds)
      min <- min(data$Seconds)
      time <- (max - min)
      return(time)
    }
    # Reaction_time<-paste("Reaction Time: " ,react_time(data),"s",sep = "")
    
    
    #Calculate the rate of change in velocity during turnover
    rateoc <- function(data,time) {
      
      initial_velocity <- data$Velocity[1]
      # Veloc_max <- athletes$Max_Vel[athletes$First_Name == input$select.player]
      delta_v <- (max(data$Velocity) - initial_velocity)
      return(delta_v/time)
    }
    
    
    #Calculate the reaction time during turnover, i.e. the time between initial velocity and maximum velocity during this turnover  
    Reaction_time<-paste("Reaction Time: " ,round(react_time(filter_player_data),2),"s",sep = "")
    
    RateofChange<-paste("Rate of Change during Reaction Time: " ,round(rateoc(filter_player_data,react_time(filter_player_data)),2),sep = "")
    #caption = cat(Reaction_time,'\n',RateofChange)
    
    if(input$select.parameters=="Velocity"){
      plot_pp<-ggplot(filter_player_data)+geom_point(mapping = aes_string(x='Seconds',y= input$select.parameters),color="red")+
        labs(x="Seconds",y= paste0(input$select.parameters),
             title = paste0(Reaction_time," ; ",RateofChange))+
        #annotate("text",x=max(filter_player_data$Seconds)-5,y=Veloc_max,label=Reaction_time)+
        #annotate("text",x=max(filter_player_data$Seconds)-5,y=Veloc_max-1,label=RateofChange)+
        geom_vline(xintercept = timein, color = '#53535B', linetype = 'dashed') +
        geom_vline(xintercept = timeout, color = '#53535B', linetype = 'dashed') +
        theme(plot.title = element_text(size = 10.5,color="#206316"))+ theme_light()+theme(plot.title = element_text(size = 10.5,color="#06A414"))
      
    }
    
    else{
      plot_pp<-ggplot(filter_player_data)+geom_point(mapping = aes_string(x='Seconds',y= input$select.parameters),color="red")+
        labs(x="Seconds",y= paste0(input$select.parameters)) +
        #annotate("text",x=max(filter_player_data$Seconds)-5,y=Veloc_max,label=Reaction_time)+
        #annotate("text",x=max(filter_player_data$Seconds)-5,y=max(filter_player_data$Velocity)-1,label=RateofChange)+
        geom_vline(xintercept = timein, color = '#53535B', linetype = 'dashed') +
        geom_vline(xintercept = timeout, color = '#53535B', linetype = 'dashed') +theme_light()
    }
    
    ggplotly(plot_pp,height = 375)
    
    
  })
  
  
  
  
  ########### CT_PERCENTAGE CHANGE PLOT ###########
  
  output$Percentage_change <- renderPlotly({
    
    starttimes_t <- starttimes[starttimes$Game_name==input$select.game,]
    game <- str_replace_all(input$select.game, "/", "_")
    
    video <- videoread(game)
    quarters <- which(!is.na(video$internal.comment)) #Looks at the filled rows marking quarter starts
    quarters <- c(quarters,nrow(video)+1)
    colnames(video)[17] <- "player"
    #colnames(video)[12] <- "CT_Type"
    video <- left_join(video,athletes,by=c("player" = "Player_number")) %>% arrange(timeIn)
    
    videosep <- vector("list",4)
    for (i in 1:4){
      videosep[[i]] <- video[quarters[i]:(quarters[i+1]-1),]
      starttime <- videosep[[i]]$timeIn[1]
      videosep[[i]] %<>% mutate(adj.timeIn=timeIn-starttime+starttimes_t[i,3],adj.timeOut=timeOut-starttime+starttimes_t[i,3])
    }
    
    filter_game_data <- aggregate[aggregate$Gamename == input$select.game,]
    filter_player_data <- filter_game_data[filter_game_data$Player == input$select.player,]
    Veloc_max <- athletes$Max_Vel[athletes$First_Name == input$select.player]
    filter_player_data["Veloc_Perc"] <- filter_player_data$Velocity / Veloc_max * 100
    
    
    timein <- videosep[[input$select.quarter]]$adj.timeIn[input$select.ctnum]
    timeout <- videosep[[input$select.quarter]]$adj.timeOut[input$select.ctnum]
    
    
    filter_player_data <-  filter_player_data %>% filter(Seconds > (timein-2) & Seconds < ( timeout +2) )
    
    pcplot <- ggplot(filter_player_data)+geom_point(mapping = aes(x=Seconds,y=Veloc_Perc),
                                                    color="blue")+ labs(x="Seconds",y="Max Velocity Percentage")+
      geom_vline(xintercept = timein, color = '#53535B', linetype = 'dashed') +
      geom_vline(xintercept = timeout, color = '#53535B', linetype = 'dashed') +
      geom_hline(yintercept = 100, color = '#57D047', linetype = 'dashed') +
      geom_hline(yintercept = 50, color = '#ED7461', linetype = 'dashed') +theme_light()
    
    ggplotly(pcplot,height = 375)
    
  })
  
  
  
  
  
  ############ User Setting Page-Athletes info Table ############
  output$tableAIO =  DT::renderDataTable({
    
    DT::datatable(athletes,options =list(searching = TRUE, pageLength = 20, scrollX = T,scrollY = "550px" ),rownames= FALSE
    )})
  
  ############Team Performance############
  output$tableTP <- DT::renderDataTable({
    
    newdata1 <- aggregate %>% left_join(athletes,by=c("Player" = "First_Name")) %>% dplyr::group_by(Player) %>% filter(Gamename == input$GameTP) %>% select(Player,Velocity, Acceleration,Max_Vel) %>% 
      summarize( Max_Velocity = max(Velocity),Max_Acceleration = max(Acceleration), Max_Velocity_Pct=  max(Velocity)/max(Max_Vel))
    
    DT::datatable(newdata1,options =list(searching = TRUE, pageLength = 18,lengthMenu = c(2, 6, 10, 14, 18), scrollX = T,scrollY = "500px"),rownames= FALSE
    ) %>% formatRound(columns = c("Max_Acceleration","Max_Velocity_Pct"),digits=2)})
  
  
}#server_end

###########-Run App-#############
shinyApp(ui,server)

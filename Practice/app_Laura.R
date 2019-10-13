## app.R ##
## Blank dashboard
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    
    
    dashboardHeader(title = "BU Field Hockey"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
            menuItem("Performance Overview", tabName = "PO", icon = icon("th")),
            menuItem("Player Analysis", tabName = "PA", icon = icon("th")),
            menuItem("Raw Data", tabName = "RD", icon = icon("th")),
            menuItem("Help", tabName = "Help", icon = icon("th"))
        )## sidebarMenu
        
    ),## dashboardSidebar
    dashboardBody(
        tabItems(
            # First tab content for Performance Overview
            tabItem(tabName = "PO",
                fluidRow(
                    column(width = 5,
                    box( title = "Game",
                         selectInput("Games", "Select Game:", 
                                     c("Game#1","Game#2","Game#3")),width = NULL), ## box 1
                    box( title = "Training",
                         selectInput("Training", "Select Training:", 
                                     c("Training#1","Training#2","Training#3")),width = NULL) ## box 2
                    ))), ## tabItem 1
            
            # Second tab content for Player Analysis
            tabItem(tabName = "PA",
                    fluidRow(
                        column(width = 5,
                               box( title = "Turn Over",
                                   width = NULL), ## box 1
                               box( title = "Player Load ",
                                   width = NULL), ## box 2
                               box( title = "GPS ",
                                    width = NULL) ## box 3
                        ))) ## tabItem 2
            ) ## tabItems
        ) ##dashboardBody
    ) ## dashboardPage

    

server <- function(input,output){}

shinyApp(ui,server)






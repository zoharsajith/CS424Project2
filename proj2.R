library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)

at <- read.csv("atlantic_storms.csv")
pt <- read.csv("pacific_storms.csv")

years <- c(2005:2018)
years <- append(years, "All")
at$years <- year(at$date)
atShort <- at[at$years > 2005,]
ID <- as.character(atShort$id)
ID <- append(ID, "All")


ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2020 Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "bruh", icon=NULL),
      menuItem("", tabName = "bruh", icon=NULL),
      menuItem("", tabName = "bruh", icon=NULL),
      menuItem("", tabName = "bruh", icon=NULL),
      menuItem("", tabName = "bruh", icon=NULL),
      menuItem("", tabName = "bruh", icon=NULL),
      
      selectInput("Year", "Select the year", years, selected = 2018),
      selectInput("ID", "Select Hurricane ID to visualize", ID, selected = "All"),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", tabName = "About")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard", 
          fluidRow(
            column(12,
                   box(title="Atlantic Map", solidHeader=TRUE, status="primary", width=6, leafletOutput("atlanticMap", height = 500)),
                   box(title="Pacific Map", solidHeader=TRUE, status="primary", width=6, leafletOutput("pacificMap", height = 500))
                   
            )
             ),
      ),
      tabItem("About", 
              h2("Project 2 of CS 424 from Prachal Patel and Zohar Sajith.\r\n
                 The Data being used was taken from the Atlantic and Northeast and North Central Hurricane Database (HURDAT2).
                 The libraries being used are shiny, shinydashboard, leaflet, etc."))
    )
  )
)


server <- function(input, output) {

  
  
  output$atlanticMap <- renderLeaflet({
    
    categoryknot <- at
    names(categoryknot)[9] = "knots"
    categoryknot$knots <- lapply(categoryknot$knots, function(x){
      if (x <= 33){
        x <- 6
      }
      else if(x >= 34 && x <= 63){
        x <- 7
      }
      else if(x >= 64 && x <= 82){
        
        x <- 1
      }
      else if(x >= 83 && x <= 95){
        
        x <- 2
      }
      else if(x >= 96 && x <= 112){
        
        x <- 3
      }
      else if(x >= 113 && x <= 136){
        
        x <- 4
      }
      else if(x >= 137){
        
        x <- 5
      }
      else{
        x <- x
      }
    })
    categoryknot2 <-categoryknot
    categoryknot2$knots <- as.character(categoryknot2$knots)
    categoryknot2$knots <- lapply(categoryknot2$knots, function(x){
      if(x == "6"){
        x <- "Tropical Depression"
      }
      else if( x == "7"){
        x <- "Tropical Storm"
      }
      else{
        x <- x
      }
    })
    names(categoryknot2)[9] = "category"
    
    
    
    
    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = categoryknot2,lng=~longitude, lat=~latitude, clusterOptions = markerClusterOptions(), popup = ~name) %>%
      addPolylines(data =categoryknot2  ,lng=~longitude, lat=~latitude)
    m
  })
  
  output$pacificMap <- renderLeaflet({
    
  })
}

shinyApp(ui = ui, server = server)
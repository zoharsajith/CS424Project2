#Read in the data and clean it up
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
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
    
    selectInput("Year", "Select the year to visualize", years, selected = 2018),
    selectInput("ID", "Select Hurricane ID to visualize", ID, selected = "All"),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", tabName = "About")
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                    leafletOutput("leaf", height = 800)
                )
              ),
              
              
              fluidRow(
                
                box(title = "Atlantic Hurricane by year", solidHeader = TRUE, status = "primary", width = 13,
                    dataTableOutput("tab0", height = 400)
                )
              ),
              fluidRow(
                
                
                box(title = "", solidHeader = TRUE, status = "primary", width = 6,
                    plotOutput("", height = 400)
                ),
                box(title = "", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("", height = 400)
                )
                
              )
                
              
      ),
      tabItem(tabName = "About",
              h2("This Project was created by Usman Siddiqui and the data is from littereli.")
      ))
    
  )
)

server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18)) 
  
  
  output$hist0 <- renderPlot({
    
    
    
  })
  
  
  # show all of the temperatures for a given room for a given year
  output$hist1 <- renderPlot({
    
    
  })
  
  
  # show a line graph of the temperatures at noon for a given room for a given year
  output$hist2 <- renderPlot({
    
  })
  
  
  
  
  # show box plot of the temperatures at noon for a given room for a given year
  output$hist3 <- renderPlot({
    
  })
  # use DT to help out with the tables - https://datatables.net/reference/option/
  output$tab0 <- DT::renderDataTable(
    DT::datatable({ 
      if( input$Year == "All" && input$ID == "All"){
        at[at$years > 2005,]
      }
      else if( input$Year != "All" && input$ID == "All"){
        atYear <- at[at$years == input$Year,]
      }
      else if( input$Year == "All" && input$ID != "All"){
        atID <- at[at$id == input$ID,]
        atID <- atID[atID$years > 2005,]
      }
      else{
        atYear <- at[at$years == input$Year,]
        atID <- atYear[atYear$id == input$ID,]
      }
      })
  )
  output$tab1 <- DT::renderDataTable(
    DT::datatable({ 
      
    })
  )
  output$tab2 <- DT::renderDataTable(
    DT::datatable({ 
    })
  )
  
  output$leaf <- renderLeaflet({
    if( input$Year == "All" && input$ID == "All")  {
      map <- leaflet( data = at[at$years > 2005,])
      Tags <- at$name
    }
    
    else if(input$ID != "All" && input$Year == "All"){
      IDMap <- at[at$id == input$ID,]
      map <- leaflet( data = IDMap[IDMap$years > 2005,])
      Tags <- IDMap$name
    }
    else if(input$ID == "All" && input$Year != "All"){
      
      dataUser <- at[at$years == input$Year,]
      map <- leaflet( data = dataUser)
      Tags <- dataUser$name
    }
    else{
      dataUser <- at[at$years == input$Year,]
      BothInput <- dataUser[dataUser$id == input$ID,]
      map <-leaflet(data = BothInput)
      Tags <- BothInput$name
    }
    map <- addTiles(map)
    map <- setView(map, lng = -10.047998, lat = 15.870, zoom = 3)
    map <- addMarkers(map, lng = ~longitude, lat = ~latitude, popup = Tags,
                      clusterOptions = markerClusterOptions())
    map
  })
}

shinyApp(ui = ui, server = server)


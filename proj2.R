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


#years <- append(years, "All")
at$years <- year(at$date)
atShort <- at[at$years > 2005,]
ID <- as.character(atShort$id)
ID <- append(ID, "All")
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
categoryknot2$category <- as.character(categoryknot2$category)
categorytable <- table(a<-(categoryknot2$category))
categorytable <- as.data.frame(categorytable)


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
doSomeReactiveThings <- reactive({subset(categoryknot2, year(categoryknot2$date) == input$Year)})
  
  getColor <- function(categoryknot){
    sapply(categoryknot$knots, function(knots){
      if(knots==1){
        "blue"
      }else if(knots==2){
        "green"
      }else if(knots==3){
        "yellow"
      }else if(knots==4){
        "orange"
      }else if(knots==5){
        "red"
      }else if(knots==6){
        "pink"
      }else{
        "purple"
      }
    })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(categoryknot)
  )


  output$atlanticMap <- renderLeaflet({
    reactiveYear <- doSomeReactiveThings()
    m <- leaflet(reactiveYear) %>% 
      addTiles() %>%
      addAwesomeMarkers(lng=~longitude, lat=~latitude, icon=icons,clusterOptions = markerClusterOptions() , popup = ~category,label =~name) 
    
    for (i in unique(categoryknot2$id)) {
      m <- m %>%
        
        addPolylines(data = reactiveYear[reactiveYear$id == i, ], 
                     lng = ~longitude, 
                     lat = ~latitude)
    }
    m <- m %>%
      addLegend(
        title = "Categories of Hurricanes",
        position = "topright",
        colors = c("Pink", "Purple", "Blue", "Green", "Yellow", "Orange", "Red"),
        labels = c("Tropical Storm", "Tropical Depression", "CAT-1", "CAT-2", "CAT-3", "CAT-4", "CAT-5"),
        opacity = 1
      )
    
    m
    

  })
  
  output$pacificMap <- renderLeaflet({
    
  })
}

shinyApp(ui = ui, server = server)
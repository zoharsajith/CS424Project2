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
library(stringr)
at <- read.csv("atlantic_storms.csv")
pa <- read.csv("pacific_storms.csv")

years <- c(2005:2018)


#years <- append(years, "All")
at$years <- year(at$date)
atShort <- at[at$years > 2005,]
ID <- as.character(atShort$id)
ID <- append(ID, "All")



#start of atlantic category code
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

#start of Pacific category code
pacategoryknot <- pa
names(pacategoryknot)[9] = "knots"

pacategoryknot$knots <- lapply(pacategoryknot$knots, function(x){
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
pacategoryknot2 <-pacategoryknot
pacategoryknot2$knots <- as.character(pacategoryknot2$knots)

pacategoryknot2$knots <- lapply(pacategoryknot2$knots, function(x){
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
names(pacategoryknot2)[9] = "category"

pacategoryknot2$category <- as.character(pacategoryknot2$category)
pacategorytable <- table(a<-(pacategoryknot2$category))
pacategorytable <- as.data.frame(pacategorytable)

#Start of atlantic hurricane table code
nodup <- at
nodup <- at[!duplicated(at["id"]),]
#nodup$Date <- as.POSIXct(x=nodup$Date, format = "%Y-%m-%d %H:%M:%S")

nodup2 <- nodup
nodup2$id <- lapply(nodup2$id, function(x){
  x <- str_sub(x,-4,-1)
})
nodup2$id <- as.character(nodup2$id)

nodup2005 <- nodup2
nodup2005 <- nodup2[nodup2005$id >= 2005,]
newyear <- table(a<-(nodup2005$id))
#creating a dataframe of unique years with amount of unique hurricanes that year
newyear <- as.data.frame(newyear)


#Start of Pacific hurricane table code
panodup <- pa
panodup <- pa[!duplicated(pa["id"]),]

#panodup$Date <- as.POSIXct(x=panodup$Date, format = "%Y-%m-%d %H:%M:%S")

panodup2 <- panodup
panodup2$id <- lapply(panodup2$id, function(x){
  x <- str_sub(x,-4,-1)
})
panodup2$id <- as.character(panodup2$id)

panodup2005 <- panodup2
panodup2005 <- panodup2[panodup2005$id >= 2005,]
panewyear <- table(a<-(panodup2005$id))
#creating a dataframe of unique years with amount of unique hurricanes that year
panewyear <- as.data.frame(panewyear)



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
          
          fluidRow(
            box(title = "Atlantic Unique Hurricanes Per Year", solidHeader = TRUE, width = 6,
                plotOutput("uniquehp", height= 400)
            ),
            box(title = "Atlantic Unique Hurricanes Per Year Table", solidHeader = TRUE, width = 6,
                DT::dataTableOutput("uniquehptb", height= 200)
            )
          ),
          fluidRow(
            box(title = "Pacific Unique Hurricanes Per Year", solidHeader = TRUE, width = 6,
                plotOutput("puniquehp", height= 400)
            ),
            box(title = "Pacific Unique Hurricanes Per Year Table", solidHeader = TRUE, width = 6,
                DT::dataTableOutput("puniquehptb", height= 200)
            )
          ),
          fluidRow(
            box(title = "Atlantic Markers Per Category", solidHeader = TRUE, width = 6,
                plotOutput("markerspc", height= 400)
            ),
            box(title = "Atlantic Markers Per Category Table", solidHeader = TRUE, width = 6,
                DT::dataTableOutput("markerspctb", height= 200)
            )
          ),
          fluidRow(
            box(title = "Pacific Markers Per Category", solidHeader = TRUE, width = 6,
                plotOutput("pmarkerspc", height= 400)
            ),
            box(title = "Pacific Markers Per Category Table", solidHeader = TRUE, width = 6,
                DT::dataTableOutput("pmarkerspctb", height= 200)
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
        colors = c("Purple","Pink", "Blue", "Green", "Yellow", "Orange", "Red"),
        labels = c("Tropical Depression", "Tropical Storm", "CAT-1", "CAT-2", "CAT-3", "CAT-4", "CAT-5"),
        opacity = 1
      )
    
    m
    

  })
  
  doSomeReactiveThings2 <- reactive({subset(pacategoryknot2, year(pacategoryknot2$date) == input$Year)})
  getColor2 <- function(pacategoryknot){
    sapply(pacategoryknot$knots, function(knots){
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
  icons2 <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(pacategoryknot)
  )
  output$pacificMap <- renderLeaflet({
    reactiveYear2 <- doSomeReactiveThings2()
    m <- leaflet(reactiveYear2) %>% 
      addTiles() %>%
      addAwesomeMarkers(lng=~longitude, lat=~latitude, icon=icons2,clusterOptions = markerClusterOptions() , popup = ~category,label =~name) 
    
    for (i in unique(pacategoryknot2$id)) {
      m <- m %>%
        
        addPolylines(data = reactiveYear2[reactiveYear2$id == i, ], 
                     lng = ~longitude, 
                     lat = ~latitude)
    }
    m <- m %>%
      addLegend(
        title = "Categories of Hurricanes",
        position = "topright",
        colors = c("Purple","Pink", "Blue", "Green", "Yellow", "Orange", "Red"),
        labels = c("Tropical Depression", "Tropical Storm", "CAT-1", "CAT-2", "CAT-3", "CAT-4", "CAT-5"),
        opacity = 1
      )
    
    m  
  })
  
  output$uniquehp <- renderPlot({
    plot2 <- ggplot(data= newyear, aes(x=Var1, y=Freq)) + 
      geom_bar(stat="identity", width=.5, color="black", fill="pink") + 
      ggtitle("Atlantic Unique Hurricanes Per Year") +
      labs(x = "Hurricanes Per Year", y = "# of Hurricanes") +
      geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25)
    plot2
  })
  
  output$uniquehptb <- DT::renderDataTable({
    newyear
    
  })
  output$puniquehp <- renderPlot({
    plot4 <- ggplot(data= panewyear, aes(x=Var1, y=Freq)) + 
      geom_bar(stat="identity", width=.5, color="black", fill="blue") + 
      ggtitle("Pacific Unique Hurricanes Per Year") +
      labs(x = "Hurricanes Per Year", y = "# of Hurricanes") +
      geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25)
    plot4
  })
  
  output$puniquehptb <- DT::renderDataTable({
    panewyear
    
  })
  
  output$markerspc <- renderPlot({
    plot3 <- ggplot(data= categorytable, aes(x=Var1, y=Freq)) + 
      geom_bar(stat="identity", width=.5, color="black", fill="pink") + 
      ggtitle("Category of Various Hurricane Points") +
      labs(x = "Categories", y = "# of frequencies") +
      geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25)
    plot3
  })
  
  output$markerspctb <- DT::renderDataTable({
    categorytable
    
  })
  
  output$pmarkerspc <- renderPlot({
    plot5 <- ggplot(data= pacategorytable, aes(x=Var1, y=Freq)) + 
      geom_bar(stat="identity", width=.5, color="black", fill="blue") + 
      ggtitle("Category of Various Hurricane Points Pacific") +
      labs(x = "Categories", y = "# of frequencies") +
      geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25)
    plot5
  })
  
  output$pmarkerspctb <- DT::renderDataTable({
    pacategorytable
    
  })
  
}

shinyApp(ui = ui, server = server)
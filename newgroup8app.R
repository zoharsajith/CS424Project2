library(shiny)
library(shinydashboard)


#Read in the data and clean it up
library(ggplot2)
library(leaflet)
library(plyr)



options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)

at <- read.table("AtlanticHurrData.txt", fill = TRUE)
pa <- read.table("pacificHurrData.txt", fill = TRUE)



names(at)[2] = "Names"
names(pa)[2] = "Names"
names(at)[3] = "Entries"
names(pa)[3] = "Entries"
names(at)[4] = "Date"
names(pa)[4] = "Date"
names(at)[5] = "Time"
names(pa)[5] = "Time"
names(at)[6] = "Record"
names(pa)[6] = "Record"
names(at)[7] = "Status"
names(pa)[7] = "Status"
names(at)[8] = "Lat"
names(pa)[8] = "Lat"
names(at)[9] = "Lon"
names(pa)[9] = "Lon"
names(at)[10] = "Max Wind"
names(pa)[10] = "Max Wind"
names(at)[11] = "Min Pres"
names(pa)[11] = "Min Pres"

at$Date <- as.POSIXct(x=at$Date, format = "%Y%m%d")
at$Time <- as.POSIXct(x=at$Time, format = "%H%M")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Hurricane Info")
      ),
      
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
              
      )
    ),
    
    fluidRow(
      box(title = "Atlantic Hurricane Info", status = "primary", solidHeader = TRUE, width = 8,leafletOutput("map", height = 400)
      ),
      box(title = "unique hurricane points", solidHeader = TRUE, width = 4,
          plotOutput("uniquehp", height= 200)
      )
      
    ),
    
    fluidRow(
      tabBox(
        title = "Hurricane Data",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Tab1", "Hurricanes Each Year"),
        tabPanel("Tab2", "Hurricanes")
      )
      )

    
  )
)

server <- function(input, output) { 
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=-30, lat=14, popup="Atlantic Ocean")
    m  # Print the map
  }) 
  
  output$tabset1Selected <- renderText({
    input$tabset1
    newat <- at
    tab <- table(a<-c(newat$V1))
    hurricanerankings <- as.data.frame(tab)
      
    hurricanerankings <- head(hurricanerankings[order(-hurricanerankings$Freq),],10 )
    names(hurricanerankings)[1] = "Unique Id"
    hurricanerankings
  })
  
  output$uniquehp <- renderPlot({
    w = table(at$V1)
    
    t = as.data.frame(w)
    
    newdata <- head(t[order(-t$Freq),], n=10)
    newdata
    m <- ggplot(data= newdata, aes(x=Var1, y=Freq)) + 
      geom_bar(stat="identity", width=.5, color="black", fill="white") + 
      ggtitle("Top 10 Hurricanes by Frequency") +
      labs(x = "Hurricane ID", y = "# Of times Spotted") +
      geom_text(aes(label = Freq), position=position_dodge(width=.9), vjust=-.25)
    m
  })
  }

shinyApp(ui, server)
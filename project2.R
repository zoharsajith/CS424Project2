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

years <- c(2005:2018)



ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2020 Example Dashboard"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                   menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   
                   selectInput("Year", "Select the year to visualize", years, selected = 2018)

                   ),
  dashboardBody()
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)


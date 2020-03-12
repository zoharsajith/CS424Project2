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


at <- read.csv("atlantic_storms.csv")
pt <- read.csv("pacific_storms.csv")


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
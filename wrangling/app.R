#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(rvest)
library(dplyr)
library(ggplot2)
library(sf)
#Get data from API

census_api_key("29617198925a35d5b0a21073bf27cd6de606a20f")
# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("American Community Survey Bureau"),
  
  # Sidebar Layout
  sidebarLayout(
    
    # SelectInput for State and Type of data
    sidebarPanel(
      
      selectInput("State", "State",
                  choices = state.abb,
                  selected = "NJ"),
      
      radioButtons("Type", "Type",
                   choices = list("median_gross_rent",
                                  "median_household_income",
                                  "ratio"), 
                   selected = "ratio")
    ),
    
    # Show a plot
    mainPanel(plotOutput("Plot"))
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  
  reduced_df <- reactive({
    get_acs(
      geography = "tract",
      variables = c(median_gross_rent = "B25064_001" , median_household_income = "B19013_001"),
      state = input$State,
      geometry = TRUE
    ) %>% .[, -5] %>% data.frame() %>% 
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = median_gross_rent / median_household_income)
  })
  
  output$Plot <- renderPlot({
    reduced_df() %>% 
      ggplot(aes_string(fill = input$Type))+ geom_sf() + ggtitle(input$Type)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

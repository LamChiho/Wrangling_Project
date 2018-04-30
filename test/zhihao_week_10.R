

library(shiny)
library(tidyverse)
library(tidycensus)
library(acs)
#Get data from API

source("api-keys.R")
census_api_key(api.key.census)
ui <- fluidPage(
  
  # Application title
  titlePanel("ACS"),
  
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
      ggplot(aes_string(fill = input$Type)) + geom_sf() + ggtitle(input$Type)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

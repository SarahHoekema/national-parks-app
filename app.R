library(shiny)
library(bslib)
library(ggplot2)
library(shinythemes)

parks <- read.csv("parks.csv", header=TRUE)
species <- read.csv("species.csv", header=TRUE)

ui <- fluidPage(
  theme = shinytheme("journal"),
  navbarPage(
    "US National Parks", 
    tabPanel("Conservation Map",
      page_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("status",
                           "Conservation Status",
                          choices = c("Species of Concern", "Threatened", "Endangered",
                                     "Extinct", "In Recovery"),
                          selected = "Species of Concern")
     
        ) 
      )
    ),
    tabPanel("Park Information"),
    tabPanel("Species Search")
  )
)


server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)

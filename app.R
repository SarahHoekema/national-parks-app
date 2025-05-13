library(shiny)
library(bslib)
library(ggplot2)
library(shinythemes)

#read in park and species tables
parks <- read.csv("parks.csv", header=TRUE)
species <- read.csv("species.csv", header=TRUE)

ui <- fluidPage(
  theme = shinytheme("journal"),
  
  #create navigation bar with various tabs
  navbarPage(
    "US National Parks", 
    #create conservation map tab 
    tabPanel("Conservation Map",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("status",
                             "Conservation Status",
                             choices = c("Species of Concern", "Threatened", "Endangered",
                                         "Extinct", "In Recovery"),
                             selected = "Species of Concern")
     
        ),
        mainPanel(
          fluidRow(
            #print map
          ),
          fluidRow(
            #print data
          )
        )
      )
    ),
    tabPanel("Park Information",
      fluidRow(
        selectizeInput("park_choice_input",
                       "Choose a park:",
                       choices = parks$Park.Name,
                       options = list(
                         placeholder = "-Select an option-",
                         onInitialize = I("function() { this.setValue(''); }")
                        )
                       )
        
      ),
      fluidRow(
        column(6,
               #create category pie chart
               ),
        column(6,
               #create native pie chart
               )
      )
    )
  )
)


server <- function(input, output) {
  

}

# Run the application 
shinyApp(ui = ui, server = server)

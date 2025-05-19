library(shiny)
library(bslib)
library(ggplot2)
library(shinythemes)
library(leaflet)
library(DT)

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
                             selected = "Species of Concern"),
          selectInput("view",
                      "Select viewing option",
                      choices = c("Data", "Graphs"),
                      selected = "Data")
        ),
        mainPanel(
          fluidRow(
            leafletOutput("map")
          ),
          conditionalPanel(
            condition = 'input.view == "Data"',
            dataTableOutput("data_table")
          ),
          conditionalPanel(
            condition = 'input.view == "Graphs"',
            column(6,
                   plotOutput("conservation_category")),
            column(6,
                   plotOutput("conservation_nativeness"))
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
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = -106.5348, lat = 41.850033, zoom = 3) |> 
      addCircleMarkers(
        data = parks,
        lng = ~Longitude,
        lat = ~Latitude,
        #layerID = ~Park.Name,
        radius = 2,
        color = "darkgreen",
        fillOpacity = 0.7,
        popup = ~paste(Park.Name)
      )
  
  })

  observeEvent(input$map_marker_click, {
    park_click <- input$map_marker_click
    park_name <- park_click[3]
    
    #species_datatable <- species |> 
      #filter(Conservation.Status %in% input$status) |> 
      #filter(Park.Name == park_name)
    
    output$data_table <- renderDataTable({
      datatable(species)
    })
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

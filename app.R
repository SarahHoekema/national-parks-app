library(shiny)
library(bslib)
library(ggplot2)
library(shinythemes)
library(leaflet)
library(DT)
library(tidyr)
library(dplyr)
library(viridis)

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
        #create sidebar
        sidebarPanel(
          #checkboxes for conservation status
          checkboxGroupInput("status",
                             "Conservation Status",
                             choices = c("Species of Concern", "Threatened", "Endangered",
                                         "Extinct", "In Recovery"),
                             selected = "Species of Concern"),
        ),
        mainPanel(
          fluidRow(
            #outputs map
            leafletOutput("map")
          ),
          fluidRow(
            #outputs data table
            dataTableOutput("data_table")
          ),
          fluidRow(
            #outputs conservation map
            plotOutput("conservation_category"))
        )
      )
    ),
    #create park information tab
    tabPanel("Park Information",
      fluidRow(
        #selectable list for parks
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
        #outputs park code
        textOutput("park_code"),
        #outputs state
        textOutput("state"),
        #outputs acres
        textOutput("acres")
      ),
      fluidRow(
        column(6,
               #outputs graph for park category
               plotOutput("park_category")
               ),
        column(6,
               #outputs graph for park nativeness
               plotOutput("park_nativeness")
               )
      )
    )
  )
)


server <- function(input, output) {
  #creates reactive map of national parks
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = -106.5348, lat = 41.850033, zoom = 3) |> 
      addCircleMarkers(
        data = parks,
        lng = ~Longitude,
        lat = ~Latitude,
        layerId = ~Park.Name,
        radius = 4,
        color = "darkgreen",
        fillOpacity = 0.7,
        popup = ~paste(Park.Name)
      )
  })
  
  #displays data table and graph if park is selected
  observeEvent({input$map_marker_click}, {
    #retrives selected park information
    park_click <- input$map_marker_click
    park_name <- park_click$id
    
    #filters species dataframe
    species_filtered_map <- species |> 
      filter(Park.Name == park_name) |> 
      filter(Conservation.Status %in% input$status) |> 
      select(Scientific.Name, Common.Names, Category, Conservation.Status)
    
    species_renamed <- species_filtered_map |> 
      rename("Scientific Name" = Scientific.Name,
             "Common Names" = Common.Names,
             "Conservation Status" = Conservation.Status)
    
    #get number of rows in dataframe
    rows <- nrow(species_filtered_map)
    
    #get counts for category
    map_category <- species_filtered_map |> 
      group_by(Category) |> 
      summarize(count = n())

    #outputs data table of selected species
    output$data_table <- renderDataTable({
      datatable(species_renamed)})

    #plots animal categories pie chart for conservation map
    if (rows != 0) {
      output$conservation_category <- renderPlot({
        ggplot(map_category, aes(x = "", y = count, fill = Category)) +
          geom_bar(stat = "identity", aes(fill = Category)) +
          coord_polar(theta = "y") +
          labs(title = "Category") +
          ylab("") + xlab("")
      })
    }
  })
  
  #shows output if park is selected
  observeEvent({input$park_choice_input}, {
    req(input$park_choice_input)
    
    #filter park dataframe by selected park
    park_filtered <- parks |> 
      filter(Park.Name == input$park_choice_input)
    
    #print park code
    output$park_code <- renderPrint({
      cat(paste("Park code: ", park_filtered$Park.Code))
      
    })
    
    #print state
    output$state <- renderPrint({
      cat(paste("State(s): ", park_filtered$State))
      
    })
    
    #print park acreage
    output$acres <- renderPrint({
      cat(paste("Acres: ", park_filtered$Acres))
      
    })
    
    #filter species dataframe by the selected park
    species_filtered_park <- species |> 
      filter(Park.Name == input$park_choice_input)
    
    #summarizes species categories
    park_category <- species_filtered_park |> 
      group_by(Category) |> 
      summarize(count = n())
    
    #summarizes species nativeness
    park_nativeness <- species_filtered_park |> 
      group_by(Nativeness) |> 
      summarize(count = n())
    
    #plots animal categories pie chart for selected park
    output$park_category <- renderPlot({
      ggplot(park_category, aes(x = "", y = count, fill = Category)) +
        geom_bar(stat = "identity", aes(fill = Category)) +
        coord_polar(theta = "y") +
        labs(title = "Category for species in park") +
        ylab("") + xlab("")
    })
      
    #plots animal nativeness pie chart for selected park
    output$park_nativeness <- renderPlot({
      ggplot(park_nativeness, aes(x = "", y = count, fill = Nativeness)) +
        geom_bar(stat = "identity", aes(fill = Nativeness)) +
        coord_polar(theta = "y") +
        labs(title = "Nativeness for species in park") +
        ylab("") + xlab("")
    })
  })

}

#run the application 
shinyApp(ui = ui, server = server)

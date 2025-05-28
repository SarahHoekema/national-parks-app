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
          #output map help text
          textOutput("map_help"),
          br(),
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
    ),
    #create park comparison tab
    tabPanel("Park Comparison",
      fluidRow(
        #selectable list for parks
        selectizeInput("park_comparison_input",
                       "Choose park(s):",
                       choices = parks$Park.Name,
                       multiple = TRUE,
                       options = list(
                        maxItems = 7,
                        placeholder = "-Select from options-",
                        onInitialize = I("function() { this.setValue(''); }")
                       )
        ),
        fluidRow(
          column(6,
                 #outputs acreage comparison plot
                 plotOutput("comparison_acreage")),
          column(6,
                 #outputs species count comparison plot
                 plotOutput("comparison_species_count"))
        )
      )
    )
  )
)


server <- function(input, output) {
  
  output$map_help <- renderPrint({
    cat("Click on a marker to select a park and scroll down to view conservation information")
    
  })
  
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
    
    #rename columns in filtered species dataframe
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
          scale_fill_viridis(discrete=TRUE) +
          labs(title = "Species Category",
               subtitle = "The categories each species falls in for the selected conservation status") +
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
    
    #remove empty nativeness rows
    park_nativeness_cleaned <- species_filtered_park[!(species_filtered_park$Nativeness == ""), ]
    park_nativeness_cleaned
    
    #summarizes species nativeness
    park_nativeness <- park_nativeness_cleaned |> 
      group_by(Nativeness) |> 
      summarize(count = n()) 
    
    #plots animal categories pie chart for selected park
    output$park_category <- renderPlot({
      ggplot(park_category, aes(x = "", y = count, fill = Category)) +
        geom_bar(stat = "identity", aes(fill = Category)) +
        coord_polar(theta = "y") +
        scale_fill_viridis(discrete=TRUE) +
        labs(title = "Species Category",
             subtitle = "The categories each species falls in for the selected park") +
        ylab("") + xlab("")
    })
      
    #plots animal nativeness pie chart for selected park
    output$park_nativeness <- renderPlot({
      ggplot(park_nativeness, aes(x = "", y = count, fill = Nativeness)) +
        geom_bar(stat = "identity", aes(fill = Nativeness)) +
        coord_polar(theta = "y") +
        scale_fill_viridis(discrete=TRUE) +
        labs(title = "Species Nativeness",
             subtitle = "The nativeness of each species for the selected park") +
        ylab("") + xlab("")
    })
    
  })
  
  observeEvent({input$park_comparison_input}, {
    req(input$park_comparison_input)
    
    #filter park dataframe for selected parks
    parks_comparison_selected <- parks |> 
      filter(Park.Name %in% input$park_comparison_input)
    
    #plot acreage bar chart
    output$comparison_acreage <- renderPlot({
      ggplot(parks_comparison_selected, aes(x = Park.Name, y = Acres)) +
        geom_bar(stat = "identity", aes(fill = Park.Name)) +
        scale_fill_viridis(discrete = TRUE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Acreage per park",
             subtitle = "A comparison of acreage between the selected parks",
             x = "Park Name",
             y = "Acres",
             fill = "Park Name")
    })
    
    #filter species dataframe for selected parks and calculate species count
    species_count <- species |> 
      filter(Park.Name %in% input$park_comparison_input) |> 
      group_by(Park.Name) |> 
      summarize(Count = n())
    
    #plot species count bar chart
    output$comparison_species_count <- renderPlot({
      ggplot(species_count, aes(x = Park.Name, y = Count)) +
        geom_bar(stat = "identity", aes(fill = Park.Name)) +
        scale_fill_viridis(discrete = TRUE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Species per park",
             subtitle = "A comparison of number of species between the selected parks",
             x = "Park Name",
             y = "Species Count",
             fill = "Park Name")
    })
      
  })
    
}

#run the application 
shinyApp(ui = ui, server = server)
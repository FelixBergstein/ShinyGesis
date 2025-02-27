# Load packages
library(openxlsx)
library(shiny)
library(dplyr)
library(readxl)

# Read the data
dt_data <- readxl::read_excel("Data/data_decision_tree.xlsx")
#setwd("/Users/jasminkoura-bodji/Desktop/Work/DecisionTreeFinal/")

# Define UI
ui <- fluidPage(
  titlePanel("DecisionTree"),
  sidebarLayout(
    sidebarPanel(
      helpText("This Decision Tree will help you find the best way to handle your Data"),
      selectInput(inputId = "col_datatype", label = "Datatype:", choices = c("", unique(dt_data$datatype))),
      uiOutput("col_perspective_ui"),
      uiOutput("col_granularity_ui"),
      uiOutput("col_targeted_ui")
    ),
    mainPanel(
      #textOutput("recommendation_authors"),
      #textOutput("recommendation_doi"),
      # display recs
      dataTableOutput("recommendations_table"), 
      downloadButton("download_table","Download Recommendation Table")
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  reactive_values <- reactiveValues(
    datatype_selected = NULL,
    perspective_selected = NULL,
    granularity_selected = NULL,
    filtered_data = NULL # storing filtered data
  )
  
  observe({
    output$col_perspective_ui <- renderUI({
      if (is.null(input$col_datatype)) return(NULL)
      selectInput(inputId = "col_perspective", label = "Perspective:", choices = c("", unique(dt_data$perspective)))
    })
    
    observeEvent(input$col_perspective, {
      reactive_values$datatype_selected <- input$col_datatype
      reactive_values$perspective_selected <- input$col_perspective
    })
    
    output$col_granularity_ui <- renderUI({
      if (is.null(reactive_values$perspective_selected)) return(NULL)
      selectInput(inputId = "col_granularity", label = "Granularity:", choices = c("", unique(dt_data$granularity)))
    })
    
    observeEvent(input$col_granularity, {
      reactive_values$granularity_selected <- input$col_granularity
    })
    
    output$col_targeted_ui <- renderUI({
      if (is.null(reactive_values$granularity_selected)) return(NULL)
      selectInput(inputId = "col_targeted", label = "Targeted:", choices = c("", unique(dt_data$targeted)))
    })
  })
  
  # Filter data based on user selections only when all inputs are available
  observe({
    # Filter data based on user selections only when all inputs are available
    reactive_values$filtered_data <- req(input$col_targeted)
    reactive_values$filtered_data <- dt_data %>%
      filter(
        if (!is.null(reactive_values$datatype_selected)) datatype == reactive_values$datatype_selected else TRUE,
        if (!is.null(reactive_values$perspective_selected)) perspective == reactive_values$perspective_selected else TRUE,
        if (!is.null(reactive_values$granularity_selected)) granularity == reactive_values$granularity_selected else TRUE,
        targeted == input$col_targeted
      )
  })
  
  
  # Render table of recommendations based on selected inputs
  output$recommendations_table <- renderDataTable({
    #print("Rendering DataTable...")
    #print(reactive_values$filtered_data)  # Print filtered data to check if it's correct
    if (is.null(reactive_values$filtered_data)) return(NULL)
    reactive_values$filtered_data %>%
      select(Authors, DOI)
  }, options = list(scrollX = TRUE, scrollY = TRUE)) # Enable scrolling
  
  
  # Download handler for recommendations table
  output$download_table <- downloadHandler(
    filename = function() {
      paste("recommendations", Sys.Date(), ".xlsx") # filename
    },
    content = function(file) {
      write.xlsx(reactive_values$filtered_data, file, rowNames = FALSE)
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)

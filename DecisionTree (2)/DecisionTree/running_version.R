# Load necessary packages
library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)




# Assume the data is preloaded or read outside the server function for simplicity in this example
# You should ensure the file path and method of loading the data fit your actual use case
dt_data <- read_excel("Data/data_decision_tree.xlsx")



# Define UI
ui <- fluidPage(
  titlePanel("Decision Tree"),
  sidebarLayout(
    sidebarPanel(
      helpText("This Decision Tree will help you find the best way to handle your Data"),
      
      # Dropdowns are immediately visible with all options available
      selectInput("col_datatype", "Datatype:", choices = c("", unique(dt_data$datatype))),
      selectInput("col_perspective", "Perspective:", choices = c("", unique(dt_data$perspective))),
      selectInput("col_granularity", "Granularity:", choices = c("", unique(dt_data$granularity))),
      selectInput("col_targeted", "Targeted:", choices = c("", unique(dt_data$targeted)))
    ),
    mainPanel(
      dataTableOutput("recommendations_table"), 
      downloadButton("download_table", "Download Recommendation Table")
    )
  )
)



# Define server logic
server <- function(input, output) {
  # Reactive value to store filtered data
 filtered_data <- reactive({
  if (input$col_datatype == "" && input$col_perspective == "" &&
      input$col_granularity == "" && input$col_targeted == "") {
    return(dt_data)  # Show all publications when no filters are selected
  }
  
  dt_data %>%
    filter(
      (input$col_datatype == "" | datatype == input$col_datatype),
      (input$col_perspective == "" | perspective == input$col_perspective),
      (input$col_granularity == "" | granularity == input$col_granularity),
      (input$col_targeted == "" | targeted == input$col_targeted)
    )
})

  
  # Dynamic UI for selecting perspective based on datatype
  output$perspective_ui <- renderUI({
    if (is.null(input$datatype) || input$datatype == "") return(NULL)
    selectInput("perspective", "Choose Perspective:", choices = c("", unique(dt_data[dt_data$datatype == input$datatype,]$perspective)))
  })
  
  # Dynamic UI for selecting granularity based on perspective
  output$granularity_ui <- renderUI({
    req(input$perspective)
    selectInput("granularity", "Choose Granularity:", choices = c("", unique(dt_data[dt_data$perspective == input$perspective,]$granularity)))
  })
  
  # Dynamic UI for selecting targeted based on granularity
  output$targeted_ui <- renderUI({
    req(input$granularity)
    selectInput("targeted", "Choose Targeted Option:", choices = c("", unique(dt_data[dt_data$granularity == input$granularity,]$targeted)))
  })
  
  # Render filtered data table
  output$table_filtered <- renderDataTable({
    filtered_data()
  }, options = list(pageLength = 5, autoWidth = TRUE)) # Customize table display options
  
  # Download handler for filtered data
  output$download_filtered <- downloadHandler(
    filename = function() { paste("filtered_data-", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      write.xlsx(filtered_data(), file)
    }
  )
}

# Run the application
shinyApp(ui, server)

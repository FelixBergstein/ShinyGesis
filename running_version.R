# Load necessary packages
library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)

# Assume the data is preloaded or read outside the server function for simplicity in this example
# You should ensure the file path and method of loading the data fit your actual use case
#dt_data <- read_excel("Data/data_decision_tree.xlsx")

dt_data <- read_excel("data_decision_tree.xlsx")


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
server <- function(input, output, session) {
  
  # Reactive dataset filtering based on user selections
  filtered_data <- reactive({
    dt_data %>%
      filter(
        if (input$col_datatype != "") datatype == input$col_datatype else TRUE,
        if (input$col_perspective != "") perspective == input$col_perspective else TRUE,
        if (input$col_granularity != "") granularity == input$col_granularity else TRUE,
        if (input$col_targeted != "") targeted == input$col_targeted else TRUE
      )
  })
  
  # Show the filtered data in a table
  output$recommendations_table <- renderDataTable({
    filtered_data()
  })
  
  # Download handler for filtered data as BibTeX
  output$download_filtered <- downloadHandler(
    filename = function() { 
      paste("filtered_data-", Sys.Date(), ".bib", sep = "") 
    },
    content = function(file) {
      writeLines("Your BibTeX content here", file)  # Replace this with actual BibTeX formatting logic
    }
  )
}







  
  


mainPanel(
  dataTableOutput("recommendations_table")  # Make sure this exists
)




# Run the application
shinyApp(ui, server)

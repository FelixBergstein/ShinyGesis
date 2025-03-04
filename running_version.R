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
      downloadButton("download_bibtex", "Download as BibTeX")
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
  
  
  
  
  output$download_bibtex <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".bib", sep = "")  # Ensure the file is named with a .bib extension
    },
    content = function(file) {
      filtered <- filtered_data()  # Assuming this is the data you filtered based on selections
      
      # Initialize an empty string to store the BibTeX content
      bibtex_content <- ""
      
      # Loop over each row to create a BibTeX entry for each record
      for (i in 1:nrow(filtered)) {
        authors <- filtered$Authors[i]
        title <- filtered$Title[i]  # Assuming you have columns for title
        doi <- filtered$DOI[i]      # Assuming you have columns for DOI
        year <- filtered$Year[i]    # Assuming you have columns for year
        
        # Create the BibTeX entry format
        bibtex_entry <- paste0(
          "@article{", gsub(" ", "", authors), year, ",\n",  # Key based on authors and year
          "  author = {", authors, "},\n",
          "  title = {", title, "},\n",
          "  year = {", year, "},\n",
          "  doi = {", doi, "},\n",
          "  journal = {Sample Journal},\n",  # Add more fields if required
          "}\n\n"
        )
        
        # Append this entry to the BibTeX content string
        bibtex_content <- paste0(bibtex_content, bibtex_entry)
      }
      
      # Write the BibTeX content to the downloaded file
      writeLines(bibtex_content, con = file)
    },
    contentType = "application/x-bibtex"  # Ensures the browser treats it as a BibTeX file
  )
  
}







  
  


mainPanel(
  dataTableOutput("recommendations_table")  # Make sure this exists
)




# Run the application
shinyApp(ui, server)

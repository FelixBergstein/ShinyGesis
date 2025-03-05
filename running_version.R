# Load necessary packages
library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)



# Assume the data is preloaded or read outside the server function for simplicity in this example
# You should ensure the file path and method of loading the data fit your actual use case
#dt_data <- read_excel("Data/data_decision_tree.xlsx")

dt_data <- read_excel("data_decision_tree.xlsx")


ui <- fluidPage(
  # Add custom CSS to style the panel
  tags$head(
    tags$style(HTML("
      .decision-tree-panel {
        background-color: #f0f0f0;  /* Light grey background */
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      }
    "))
  ),
  
  navbarPage(
    title = "Data Analysis",
    
    # Decision Tree tab
    tabPanel("Decision Tree", 
             # Split the layout into two rows
             fluidRow(
               # Left column for both bar charts (one-third of the page width)
               column(4, 
                      plotOutput("bar_chart"),  # First graph here
                      plotOutput("bar_chart_2")  # Second graph here
               ),
               
               # Right column for the decision tree options and table (two-thirds of the page width)
               column(8, 
                      # Decision Tree Panel (Options at the top)
                      div(
                        class = "decision-tree-panel",
                        helpText("This Decision Tree will help you find the best way to handle your Data"),
                        fluidRow(
                          column(4, selectInput("col_datatype", "Datatype:", choices = c("", unique(dt_data$datatype)))),
                          column(4, selectInput("col_perspective", "Perspective:", choices = c("", unique(dt_data$perspective)))),
                          column(4, radioButtons("col_granularity", "Granularity:", choices = c("1", "2"), inline = TRUE))  # Horizontal radio buttons
                        )
                      ),
                      
                      # Below the decision tree, place the table
                      dataTableOutput("recommendations_table")
               )
             ),
             
             # First download button below the graph/table
             downloadButton("download_bibtex", "Download as BibTeX")
    ),
    
    # Background tab (empty for now)
    tabPanel("Background", 
             h3("Background content will be added here.")
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
        if (input$col_granularity != "") granularity == input$col_granularity else TRUE
      )
  })
  
  # Render the bar chart (Independent of the decision tree)
  output$bar_chart <- renderPlot({
    # Use the entire dataset without filtering based on decision tree options
    ggplot(dt_data, aes(x = datatype, fill = factor(granularity))) +
      geom_bar(position = "stack", color = "black") +  # Bar color and border
      labs(title = "Distribution of papers by Datatype and Granularity", 
           x = "Datatype", 
           y = "Number of Cases") +
      scale_fill_manual(values = c("1" = "#3498db", "2" = "#e74c3c"),  # Define two colors for Granularity 1 and 2
                        name = "Granularity", 
                        labels = c("Granularity 1", "Granularity 2")) +
      theme_minimal() +  # Minimal theme for the plot
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
  })
  
  # Render the second bar chart (Distribution by Perspective)
  output$bar_chart_2 <- renderPlot({
    # Use the entire dataset (not affected by decision tree) for the second graph
    ggplot(dt_data, aes(x = perspective, fill = factor(granularity))) +
      geom_bar(position = "stack", color = "black") +  # Bar color and border
      labs(title = "Distribution of Perspectives by Granularity", 
           x = "Perspective", 
           y = "Number of Cases") +
      scale_fill_manual(values = c("1" = "#3498db", "2" = "#e74c3c"),  # Define two colors for Granularity 1 and 2
                        name = "Granularity", 
                        labels = c("Granularity 1", "Granularity 2")) +
      theme_minimal() +  # Minimal theme for the plot
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
  })
  
  
  
  # Show the filtered data in a table
  output$recommendations_table <- renderDataTable({
    filtered_data() %>%
      select(Title, Authors, DOI)  # Select only the columns you want to display
  })
  
  
  
  
  
  output$download_bibtex <- downloadHandler(
    filename = function() { paste("recommendations_", Sys.Date(), ".bib", sep = "") },
    content = function(file) {
      # Creating the BibTeX entry string for each row
      bibtex_entries <- apply(filtered_data(), 1, function(row) {
        authors <- gsub("'", "''", row["Authors"])  # Replace single quotes with double quotes
        doi <- row["DOI"]
        id <- row["ID"]
        title <- row["Title"]  # Assuming you have a Title column
        
        # Ensure proper BibTeX formatting
        entry <- paste0(
          "@article{", 
          id, ",\n",  # Use the ID as the citation key
          "  author = {", authors, "},\n",
          "  title = {", title, "},\n",  # Add the title from the dataset
          "  journal = {No journal information},\n",  # Add a placeholder journal field
          "  year = {No year information},\n",  # Add a placeholder year field
          "  doi = {", doi, "}\n",
          "}\n"
        )
        
        return(entry)
      })
      
      # Write the formatted BibTeX entries to the file
      writeLines(bibtex_entries, file)
    }
  )
  
}







  
  


mainPanel(
  dataTableOutput("recommendations_table")  # Make sure this exists
)




# Run the application
shinyApp(ui, server)

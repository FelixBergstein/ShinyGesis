library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(DT)

# Load Data
dt_data <- read_excel("data_decision_tree.xlsx")

# Define UI
ui <- navbarPage(
  title = "Data Analysis",
  
  # Decision Tree Tab
  tabPanel("Decision Tree", 
           fluidPage(
             fluidRow(
               column(4, 
                      div(class = "bar-chart", plotOutput("bar_chart")),  
                      div(class = "bar-chart", plotOutput("bar_chart_2"))
               ),
               column(8, 
                      div(
                        class = "decision-tree-panel",
                        helpText("This Decision Tree will help you find the best way to handle your Data"),
                        fluidRow(
                          column(4, 
                                 div(style="display: flex; flex-direction: column; align-items: center;",
                                     selectInput("col_datatype", "Datatype:", choices = c("", unique(dt_data$datatype))),
                                     actionButton("clear_datatype", "Clear")
                                 )
                          ),
                          column(4, 
                                 div(style="display: flex; flex-direction: column; align-items: center;",
                                     selectInput("col_perspective", "Perspective:", choices = c("", unique(dt_data$perspective))),
                                     actionButton("clear_perspective", "Clear")
                                 )
                          )
                          ,
                          column(4, 
                                 div(style="display: flex; flex-direction: column; align-items: center;",
                                     radioButtons("col_granularity", 
                                                  "Granularity:", 
                                                  choices = c("-" = "", "1", "2"),  
                                                  inline = TRUE, 
                                                  selected = ""),
                                     actionButton("clear_granularity", "Clear")
                                     
                                 )
                          )
                        ))
                        ,
                      DT::DTOutput("recommendations_table"),
                      downloadButton("download_bibtex", "Download as BibTeX")
               )
             )
           )
  ),
  
  # Background Tab
  tabPanel("Background",
           fluidPage(
             fluidRow(
               column(8,
                      h2(id = "introduction", "Introduction"),
                      p("This section will provide an introduction to the project."),
                      
                      h2(id = "project_description", "Project Description"),
                      p("Here, we will describe the purpose and scope of the project."),
                      
                      h2(id = "paper_description", "Paper Description"),
                      p("This section will outline the main paper used in the analysis."),
                      
                      h2(id = "decision_tree", "Decision Tree"),
                      p("Explanation of how the decision tree works.")
               ),
               column(4,
                      wellPanel(
                        h4("Table of Contents"),
                        tags$ul(
                          tags$li(tags$a(href = "#introduction", "Introduction")),
                          tags$li(tags$a(href = "#project_description", "Project Description")),
                          tags$li(tags$a(href = "#paper_description", "Paper Description")),
                          tags$li(tags$a(href = "#decision_tree", "Decision Tree"))
                        )
                      )
               )
             )
           )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive dataset filtering
  filtered_data <- reactive({
    dt_data %>%
      filter(
        (input$col_datatype == "" | datatype == input$col_datatype) &
          (input$col_perspective == "" | perspective == input$col_perspective) &
          (input$col_granularity == "" | granularity == input$col_granularity)
      )
  })
  

  output$recommendations_table <- renderDataTable({
    req(filtered_data())  # Ensure filtered_data() is not NULL
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  
  # Bar Chart 1
  output$bar_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = datatype, fill = factor(granularity))) +
      geom_bar(position = "stack", color = "black") +  
      labs(title = "Distribution of Datatypes by Granularity", 
           x = "Datatype", 
           y = "Number of Cases") +
      scale_fill_manual(values = c("1" = "#3498db", "2" = "#e74c3c"),  
                        name = "Granularity", 
                        labels = c("1", "2")) +  
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  
            plot.title = element_text(face = "bold"))  
  })
  
  # Bar Chart 2
  output$bar_chart_2 <- renderPlot({
    ggplot(filtered_data(), aes(x = perspective, fill = factor(granularity))) +
      geom_bar(position = "stack", color = "black") +  
      labs(title = "Distribution of Perspectives by Perspective", 
           x = "Perspective", 
           y = "Number of Cases") +
      scale_fill_manual(values = c("1" = "#3498db", "2" = "#e74c3c"),  
                        name = "Granularity", 
                        labels = c("1", "2")) +  
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  
            plot.title = element_text(face = "bold"))  
  })
  
  # Download BibTeX
  output$download_bibtex <- downloadHandler(
    filename = function() { paste("recommendations_", Sys.Date(), ".bib", sep = "") },
    content = function(file) {
      bibtex_entries <- apply(filtered_data(), 1, function(row) {
        authors <- if ("Authors" %in% colnames(dt_data)) row["Authors"] else "Unknown"
        doi <- if ("DOI" %in% colnames(dt_data)) row["DOI"] else "No DOI"
        id <- if ("ID" %in% colnames(dt_data)) row["ID"] else paste0("entry", sample(1000:9999, 1))
        title <- if ("Title" %in% colnames(dt_data)) row["Title"] else "Untitled"
        
        paste0(
          "@article{", 
          id, ",\n",  
          "  author = {", authors, "},\n",
          "  title = {", title, "},\n",  
          "  journal = {No journal information},\n",  
          "  year = {No year information},\n",  
          "  doi = {", doi, "}\n",
          "}\n"
        )
      })
      writeLines(bibtex_entries, file)
    }
  )
  
  # Reset selection buttons
  observeEvent(input$clear_datatype, {
    updateSelectInput(session, "col_datatype", selected = "")
  })
  observeEvent(input$clear_perspective, {
    updateSelectInput(session, "col_perspective", selected = "")
  })
  observeEvent(input$clear_granularity, {
    updateRadioButtons(session, "col_granularity", selected = "")
  })
}

shinyApp(ui, server)

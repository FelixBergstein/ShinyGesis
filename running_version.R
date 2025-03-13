library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(DT)

# Load Data
dt_data <- read_excel("data/data_decision_tree.xlsx")

# Define UI
ui <- navbarPage(
  title = "Gesis",
  
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
                        div(style="border: 1px solid #ccc; background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
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
                              ),
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
                            )
                        )
                      ),
                      DT::DTOutput("recommendations_table"),
                      downloadButton("download_bibtex", "Download as BibTeX")
               )
             )
           )
  ),
  
  # Background Tab
  tabPanel("Background",
           fluidPage(
             tags$head(
               tags$style(HTML("
                 p {
                   text-align: justify;
                 }
               "))
             ),
             fluidRow(
               column(8,
                      h2(id = "introduction", "Introduction"),
                      p("This section will provide an introduction to the project."),
                      
                      h2(id = "project_description", "Project Description"),
                      p("Here, we will describe the purpose and scope of the project."),
                      
                      h2(id = "paper_description", "Paper Description"),
                      p("This section will outline the main paper used in the analysis."),
                      
                      h2(id = "decision_tree", "Decision Tree"),
                      p("The decision tree allows you to filter the 53 cited papers according to your needs. Firstly, you may filter based on what Datatype the work is about. 
                      They are sorted by Register, Sensor, Social Media, Survey and Text data, as well as untargeted papers.", br(),  
                        "Secondly, you can filter the papers based on their perspective on the subject, which is sorted into Data, User, Data and User, Analytical frames and Challenges.", br(),  
                        "Lastly, you can filter the papers based on their granularity, which is binary at 1 or 2.", br(),  
                        "After having chosen what factors to filter the papers by, you can download the selection as a BibTeX file for citation.")
               ),
               
               
               column(4,
                      wellPanel(
                        tags$head(
                          tags$style(HTML("
             .toc-list {
               padding-left: 0;
             }
             .toc-list li {
               border-bottom: 1px solid black;
               list-style-type: none;
               font-size: 14px;
               padding: 5px 10px;
               transition: background-color 0.2s ease-in-out;
             }
             .toc-list li:last-child {
               border-bottom: none;
             }
             .toc-list li:hover {
               background-color: lightgrey;
             }
             .toc-list li a {
               text-decoration: none;
               color: black;
               display: block;
               width: 100%;
             }
             .well {
               background-color: transparent !important;
               border: none;
               box-shadow: none;
               padding: 10px;
             }
           "))
                        ),
                        h4("Table of Contents", style = "font-size: 16px; font-weight: bold;"),
                        tags$ul(class = "toc-list",
                                tags$li(tags$a(href = "#introduction", "1. Introduction")),
                                tags$li(tags$a(href = "#project_description", "2. Project Description")),
                                tags$li(tags$a(href = "#paper_description", "3. Paper Description")),
                                tags$li(tags$a(href = "#decision_tree", "4. Decision Tree"))
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
  

  # Table Output
  output$recommendations_table <- renderDataTable({
    filtered_data() %>%
      select(Title, Authors, DOI)  
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

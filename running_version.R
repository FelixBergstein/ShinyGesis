library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(DT)
library(tidyverse)

# Load Data
dt_data <- read_excel("data/data_v2.xlsx") |> 
  rename("Untargeted"= dtype_untargeted,
    "Survey Data" = dtype_survey , 
          "Sensor Data" = dtype_sensor,
          "Social Media Data" = dtype_wsm,
          "Visual Data" = dtype_visual,
          "Register Data" = dtype_register)


# Define UI
ui <- navbarPage(
  title = "Data Quality",
  
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
                        helpText("Filter the list of Data Quality Frameworks using the drop-down filters below:"),
                        div(style="border: 1px solid #ccc; background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                            fluidRow(
                              column(4, 
                                     div(style="display: flex; flex-direction: column; align-items: center;",
                                         selectInput("col_datatype", "Datatype:", choices = c("","Untargeted", "Social Media Data", "Visual Data", "Survey Data", "Sensor Data", "Register Data")),
                                         actionButton("clear_datatype", "Clear")
                                     )
                              ),
                              column(4, 
                                     div(style="display: flex; flex-direction: column; align-items: center;",
                                         selectInput("col_perspective", "Perspective:", choices = c("", "extrinsic", "intrinsic")),
                                         actionButton("clear_perspective", "Clear")
                                     )
                              ),
                              column(4, 
                                     div(style="display: flex; flex-direction: column; align-items: center;",
                                         selectInput("col_granularity", "Perspective:", choices = c("", "general", "specific")),
                                         
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
        (input$col_datatype == "" | case_when(
          input$col_datatype == "Untargeted" ~ `Untargeted` == 1,
          input$col_datatype == "Social Media Data" ~ `Social Media Data` == 1,
          input$col_datatype == "Visual Data" ~ `Visual Data` == 1,
          input$col_datatype == "Survey Data" ~ `Survey Data` == 1,
          input$col_datatype == "Sensor Data" ~ `Sensor Data` == 1,
          input$col_datatype == "Register Data" ~ `Register Data` == 1,
          TRUE ~ TRUE # Allow all if no filter is applied
        )) &
          (input$col_perspective == "" | case_when(
            input$col_perspective == "extrinsic" ~ dummy_extrinsic == 1,
            input$col_perspective == "intrinsic" ~ dummy_intrinsic == 1,
            TRUE ~ TRUE
          )) &
          (input$col_granularity == "" | dummy_granularity == input$col_granularity)
      )
  })
  
  
  

  # Table Output
  output$recommendations_table <- renderDataTable({
    filtered_data() %>%
      mutate(
        meta_doi = ifelse(!is.na(meta_doi) & meta_doi != "",
                          paste0("<a href='https://doi.org/", meta_doi, "' target='_blank'>", meta_doi, "</a>"),
                          NA)
      ) %>%
      dplyr::select(Title = meta_title, Authors = meta_authors, Link = meta_doi) %>%
      datatable(escape = FALSE, options = list(pageLength = 10))
  })
  
  
  # Bar Chart 1: Data Type Distribution
  output$bar_chart <- renderPlot({
    # Get the filtered dataset
    data_long <- filtered_data() %>%
      select(Untargeted,`Social Media Data`, `Register Data`, `Survey Data`, `Sensor Data`, `Visual Data`) %>%
      mutate_all(~replace_na(., 0)) %>%  # Replace NA with 0
      pivot_longer(cols = everything(), names_to = "datatype", values_to = "value") %>%
      mutate(value = as.numeric(value)) %>%  # Ensure numeric values
      filter(value == 1) %>%
      count(datatype)
    
    # Prevent ggplot errors when no data is available
    if (nrow(data_long) == 0) {
      return(NULL)
    }
    
    ggplot(data_long, aes(x = reorder(datatype, n), y = n, fill = datatype)) +
      geom_bar(stat = "identity") +
      coord_flip() +  # Flip bars for better readability
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1")+
      labs(title = "Distribution of Data Types", x = "Data Type", y = "Count") +
      theme(legend.position = "none")
  })
  
  
  # Bar Chart 2: Perspective Distribution (Now Uses the Same Filtered Dataset)
  output$bar_chart_2 <- renderPlot({
    # Get the filtered dataset
    data_perspective <- filtered_data() %>%
      select(dummy_extrinsic, dummy_intrinsic) %>%
      pivot_longer(cols = everything(), names_to = "perspective", values_to = "value") %>%
      filter(value == 1) %>%
      count(perspective)
    
    # Prevent ggplot errors when no data is available
    if (nrow(data_perspective) == 0) {
      return(NULL)
    }
    
    ggplot(data_perspective, aes(x = perspective, y = n, fill = perspective)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_brewer(palette = "Greys")+
      labs(title = "Distribution of Perspectives", x = "Perspective", y = "Count") +
      theme(legend.position = "none")
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

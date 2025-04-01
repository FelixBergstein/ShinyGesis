library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(DT)
library(tidyverse)

#Things to do/ to fix.
# 1.Granularity legend for bar chart
# 2. Add Gesis colors.



# Load Data
dt_data <- read_excel("data/data_v3.xlsx") |>
  rename("Untargeted"= dtype_untargeted,
    "Survey Data" = dtype_survey ,
          "Sensor Data" = dtype_sensor,
          "Social Media Data" = dtype_wsm,
          "Visual Data" = dtype_visual,
          "Register Data" = dtype_register)


# Define UI
ui <- navbarPage(
  title = "Data Quality",

  # Custom CSS for navbar hover effects
  header = tags$head(
    tags$style(HTML("
    /* Set the entire navbar background to white */
    .navbar {
      background-color: white !important;
    }

    /* Ensure navbar title remains black */
    .navbar-brand {
      color: black !important;
    }

    /* Style all tabs normally */
    .navbar-nav .nav-item .nav-link {
      color: black !important;
      padding: 10px 15px;
      transition: background-color 0.3s ease-in-out, color 0.3s ease-in-out;
    }

    /* Hover effect: change background color and keep text black */
    .navbar-nav .nav-item .nav-link:hover {
      background-color: #f4d4e1 !important; /* Light pink */
      color: black !important;
      border-radius: 5px;
    }

    /* Active tab: different background color with white text */
    .navbar-nav .nav-item .active {
      background-color: #d20064 !important;
      color: white !important;
      border-radius: 5px;
    }
  "))
  ),



  # Background Tab
  tabPanel(tags$span(style = "color: #920948;", tags$b("Background")),
           fluidPage(
             tags$head(
               tags$style(HTML("
                 p {
                   text-align: justify;
                 }
               "))
             ),
             fluidRow(
               column(5, offset = 3,
                      h1(id = "introduction", style = "color: #1E8CC8;", "Welcome"),
                      p("This app provides tools that you can use to


                        project provides a comprehensive analysis of data quality in digital social research, highlighting the challenges and strategies involved in ensuring reliable results. It features a decision tree that allows users to filter and download relevant papers cited in the research. The decision tree makes it easy to explore the 53 papers referenced in the study based on specific criteria, including datatype, perspective, and granularity. Whether you're a researcher looking for targeted insights or simply interested in understanding the state of digital social research, this tool offers a convenient way to navigate through the literature. The papers available for download are organized to meet the needs of various research contexts."),


                      h2(id = "paper_description", style = "color: #1E8CC8;", "Paper Description"),
                      p("In the age of digital social research, ensuring the quality of data is more important than ever. As researchers increasingly rely on digital data sources like social media, web scraping, and mobile applications, it is crucial to understand the challenges that come with using such data. This research explores the various dimensions of data quality, including accuracy, completeness, consistency, and validity, and identifies the key issues researchers face in these areas. One of the major concerns is the presence of bias, noise, and the representativeness of digital data, which can significantly impact the reliability of findings.

To address these challenges, we discuss effective strategies for improving data quality, such as data cleaning, validation techniques, and triangulating with traditional research methods. Transparency in data collection and analysis is essential, and researchers must disclose their methodologies and data sources to ensure trustworthiness. Ethical considerations, particularly regarding privacy, consent, and data ownership, are also critical when working with digital data.

Looking forward, this research calls for the development of standardized metrics and guidelines to assess data quality in digital social research. An interdisciplinary approach, combining insights from social science, computer science, and data ethics, is needed to navigate the complexities of digital data. By improving data quality assessment frameworks, we aim to provide researchers with the tools they need to conduct more reliable and ethical digital social research."),

                      h2(id = "decision_tree", style = "color: #1E8CC8;", "Decision Tree"),
                      p("The decision tree allows you to filter the 58 cited papers according to your needs. The steps are the following: "),

                       tags$ol(
                        tags$li("Filter by what", tags$b("Data Type"), "the publications are about.", tags$br(), "They are sorted into Register, Sensor, Social Media, Survey and Text data, as well as untargeted papers", tags$br(),tags$br()),
                        tags$li("Filter the publications based on their", tags$b("Perspective"), "on the subject.", tags$br(), "They are sorted into Data, User, Data and User, Analytical frames and Challenges.", tags$br(),tags$br()),
                        tags$li("Lastly, you can filter the papers based on their ", tags$b("Granularity."), tags$br(), "They are sorted into general and specific.")
                      ),

                      p("After having chosen what factors to filter the papers by, you can download the selection as a BibTeX file for citation."),

               h2(id = "evidence_gap_map", style = "color: #1E8CC8;", "Evidence Gap Map"),
               p("The Evidenve Gap Map illustrates to what extent which papers discuss different types of Errors, based on their Data Type. The Errors covered by the Map are:"),

               tags$ul(
                 tags$li(
                   tags$b("Representation"), "Errors",
                   tags$ul(
                     tags$li("Coverage Error"),
                     tags$li("Sampling Error"),
                     tags$li("Nonresponse Error")
                   )
                 ),
                 tags$li(
                   tags$b("Measurement"), "Errors",
                   tags$ul(
                     tags$li("Content Validity Error"),
                     tags$li("Measurement Error Response"),
                     tags$li("Measurement Error Platform"),
                     tags$li("Preprocessing Error")

                   )
                 ),
                 tags$li(
                   tags$b("Modelling"), "Errors",
                 )
               ),

               p("The Circle Size indicates the number of publications covering each type of Error. The exact amount is shows when hovering over the circle."),

               ),





               column(2,
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
                        h4("Table of Contents", style = "font-size: 16px; color: #1E8CC8"),
                        tags$ul(class = "toc-list",
                                tags$li(tags$a(href = "#introduction", "1. Introduction")),
                                tags$li(tags$a(href = "#paper_description", "2. Paper Description")),
                                tags$li(tags$a(href = "#decision_tree", "3. Decision Tree")),
                                tags$li(tags$a(href = "#evidence_gap_map", "4. Evidence Gap Map"))
                        )
                      )
               )



             )
           )
  ),
  # Decision Tree Tab
  tabPanel(tags$span(style = "color: #920948;", tags$b("Decision Tree")),
           div(
             style = "width: 100%; text-align: center; margin-bottom: 20px;",
             h2( style = "color: #1E8CC8;", "Desicion Tree"),
             div("The desicion tree helps you to identify relevant data quality frameworks for a specific research context", style = "font-size: 14px;")
           ),
           fluidPage(
             fluidRow(
               column(4,
                      div(class = "bar-chart", plotOutput("bar_chart"), uiOutput("bar_chart_desc")),
                      div(class = "paper-count", uiOutput("paper_count"),
                          style = "margin-top: 100px;")

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
                                         selectInput("col_granularity", "Granularity:", choices = c("", "general", "specific")),

                                         actionButton("clear_granularity", "Clear")
                                     )
                              )
                            )
                        )
                      ),
                      downloadButton("download_bibtex", "Download as BibTeX"),
                      br(), br(),

                      DT::DTOutput("recommendations_table")
               )
             )
           )
  ),


  tabPanel(tags$span(style = "color: #920948;", tags$b("Evidence Gap Map")),
           div(
             style = "width: 100%; text-align: center; margin-bottom: 20px;",
             h2( style = "color: #1E8CC8;", "Evidence Gap Map"),
             div("The evidence gap map highlights the social science data types and quality dimensions which are already addressed in the available frameworks.", style = "font-size: 14px;")
           ),
           tags$iframe(
             src = "map_final.html",
             width = "100%",
             height = "1000px",
             style = "border:none; overflow:auto;"
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


  output$bar_chart <- renderPlot({
    # Get the filtered dataset based on decision tree
    data_long <- filtered_data() %>%
      select(Untargeted, `Social Media Data`, `Register Data`, `Survey Data`, `Sensor Data`, `Visual Data`, dummy_granularity) %>%
      mutate_all(~replace_na(., 0)) %>%  # Replace NA with 0
      pivot_longer(cols = -dummy_granularity, names_to = "datatype", values_to = "value") %>%
      mutate(value = as.numeric(value)) %>%  # Ensure numeric values
      filter(value == 1) %>%
      mutate(granularity = factor(ifelse(dummy_granularity == "specific", "Specific", "General"), levels = c("General", "Specific"))) %>%
      count(datatype, granularity)

    # Fix the order of 'datatype' factor and reverse it
    data_long$datatype <- factor(data_long$datatype, levels = c("Sensor Data", "Social Media Data", "Register Data", "Untargeted", "Survey Data"))

    # Prevent ggplot errors when no data is available
    if (nrow(data_long) == 0) {
      return(NULL)
    }

    # Plot the bar chart with fixed and reversed order of data types
    ggplot(data_long, aes(x = datatype, y = n, fill = interaction(datatype, granularity, sep = "."))) +
      geom_bar(stat = "identity", position = "stack") +
      coord_flip() +  # Flip bars for better readability
      theme_minimal() +
      scale_fill_manual(values = c(
        "Untargeted.General" = "#ffc3e0", "Untargeted.Specific" = "#d20064",
        "Social Media Data.General" = "#e5cbed", "Social Media Data.Specific" = "#642878",
        "Register Data.General" = "#e2f2fb", "Register Data.Specific" = "#1e8cc8",
        "Survey Data.General" = "#A5D1E9", "Survey Data.Specific" = "#003c78",
        "Sensor Data.General" = "#C7E2F1", "Sensor Data.Specific" = "#105F94",
        "Visual Data.General" = "#ED99C1", "Visual Data.Specific" = "#920948"
      )) +
      labs(title = "Distribution of Data Types with Granularity", x = "Data Type", y = "Count") +
      theme(legend.position = "none",  # Remove the legend
            plot.title = element_text(size = 19)
)
  })

  # Render the description below the chart
  output$bar_chart_desc <- renderUI({
    HTML("<p style='text-align: center; font-size: 14px; color: gray;'>Darker colors indicate specific granularity, lighter colors general granularity.</p>")
  })















  # Paper Count Display
  output$paper_count <- renderUI({
    count <- nrow(filtered_data())  # Get the number of filtered papers

    div(style = "border: 2px solid black; padding: 20px; border-radius: 10px; text-align: center; width: 200px; margin: auto;",
        div(style = "font-size: 40px; font-weight: bold;", count),
        div(style = "font-size: 14px; color: grey; margin-top: 5px;", "Papers Found")
    )
  })



  # Download BibTeX
  output$download_bibtex <- downloadHandler(
    filename = function() {
      "references.bib"
    },
    content = function(file) {
      bibtex_data <- filtered_data() %>%
        select(bibtex) %>%
        mutate(bibtex = paste0("@", bibtex))  # Add @ manually

      writeLines(bibtex_data$bibtex, file)
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

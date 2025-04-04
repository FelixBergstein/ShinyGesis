library(shiny)
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(DT)
library(tidyverse)
library(shinyjs)

# Load Data
dt_data <- read_excel("data/data_v3.xlsx") |>
  rename(
    "Untargeted" = dtype_untargeted,
    "Survey Data" = dtype_survey,
    "Sensor Data" = dtype_sensor,
    "Social Media Data" = dtype_wsm,
    "Visual Data" = dtype_visual,
    "Register Data" = dtype_register
  )

# Define UI
ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: white !important;
      }
      .navbar-brand {
        color: black !important;
      }
      .navbar-nav .nav-item .nav-link {
        color: black !important;
        padding: 10px 15px;
        transition: background-color 0.3s ease-in-out, color 0.3s ease-in-out;
      }
      .navbar-nav .nav-item .nav-link:hover {
        background-color: #f4d4e1 !important;
        color: black !important;
        border-radius: 5px;
      }
      .navbar-nav .nav-item .active {
        background-color: #d20064 !important;
        color: white !important;
        border-radius: 5px;
      }
    "))
  ),

  navbarPage(
    title = "Data Quality",
    id = "main_navbar",  # Enable tab switching

    tabPanel(tags$span(style = "color: #920948;", tags$b("Background")),
             fluidPage(
               tags$head(
                 tags$style(HTML("p { text-align: justify; }"))
               ),
               fluidRow(
                 column(8,
                        h1(id = "introduction", style = "color: #1E8CC8;", "Welcome"),
                        p(HTML(
                          'This app provides tools that you can use to explore data quality in digital social research.
  It features a <strong>Decision Tree</strong> and an <strong>Evidence Gap Map</strong> to help you filter and review 58 cited papers based on criteria such as data type, perspective, and granularity.'
                        ))
                        ,

                        h2(id = "paper_description", style = "color: #1E8CC8;", "Paper Description"),
                        p("In the age of digital social research, ensuring the quality of data is more important than ever. As researchers increasingly rely on digital data sources like social media, web scraping, and mobile applications, it is crucial to understand the challenges that come with using such data. This research explores the various dimensions of data quality, including accuracy, completeness, consistency, and validity, and identifies the key issues researchers face in these areas. One of the major concerns is the presence of bias, noise, and the representativeness of digital data, which can significantly impact the reliability of findings.

To address these challenges, we discuss effective strategies for improving data quality, such as data cleaning, validation techniques, and triangulating with traditional research methods. Transparency in data collection and analysis is essential, and researchers must disclose their methodologies and data sources to ensure trustworthiness. Ethical considerations, particularly regarding privacy, consent, and data ownership, are also critical when working with digital data.

Looking forward, this research calls for the development of standardized metrics and guidelines to assess data quality in digital social research. An interdisciplinary approach, combining insights from social science, computer science, and data ethics, is needed to navigate the complexities of digital data. By improving data quality assessment frameworks, we aim to provide researchers with the tools they need to conduct more reliable and ethical digital social research."),

                        tabsetPanel(
                          tabPanel("Decision Tree",
                                   tags$div(
                                     style = "padding-top: 10px;",
                                     h3(style = "color: #1E8CC8;", "How to use the Decision Tree"),
                                     tags$p(HTML(
                                       "The <span style='color:#1E8CC8; cursor:pointer; text-decoration:underline;' id='link_to_tree'>Decision Tree</span> serves as an initial guide to navigating through the various data quality frameworks.

        Since decision trees are commonly utilized to facilitate decision-making in complex and high-dimensional scenarios, they enable researchers to choose frameworks that best suit their specific research problem."
                                     )),
                                     br(),
                                     tags$ol(
                                       tags$li(tags$b("Data Type:"), " Register, Sensor, Social Media, Survey, Text, or Untargeted"),
                                       tags$li(tags$b("Perspective:"), " Data-centric, User-centric, Combined, Analytical frames, or Challenges"),
                                       tags$li(tags$b("Granularity:"), " General or Specific")
                                     ),
                                     p("Once filtered, you can download selected citations as a BibTeX file.")
                                   )
                          ),
                          tabPanel("Evidence Gap Map",
                                   tags$div(
                                     style = "padding-top: 10px;",
                                     h3(style = "color: #1E8CC8;", "How to use the Evidence Gap Map"),
                                     tags$p(HTML("The <span style='color:#1E8CC8; cursor:pointer; text-decoration:underline;' id='link_to_map'>Evidence Gap Map</span> serves as a detailed guide of error sources and data types targeted by the (intrinsic) data quality frameworks. It presents the selected error types for social science data on the y-axis, mapping them against selected data types on the x-axis. The size of the bubble represents how many frameworks include the respective error source by data type. The errors covered are:")),
                                     tags$ul(
                                       tags$li(tags$b("Representation Errors"), tags$ul(
                                         tags$li("Coverage Error"),
                                         tags$li("Sampling Error"),
                                         tags$li("Nonresponse Error")
                                       )),
                                       tags$li(tags$b("Measurement Errors"), tags$ul(
                                         tags$li("Content Validity Error"),
                                         tags$li("Measurement Error (Response)"),
                                         tags$li("Measurement Error (Platform)"),
                                         tags$li("Preprocessing Error")
                                       )),
                                       tags$li(tags$b("Modelling Errors"))
                                     ),
                                     p("Larger circles mean more publications on the topic. Hover over the bubbles to see the exact counts.")
                                   )
                          )
                        )
                 ),



               )
             )
    ),

    tabPanel(tags$span(style = "color: #920948;", tags$b("Decision Tree")),
             value = "decision_tree",
             div(
               style = "width: 100%; text-align: center; margin-bottom: 20px;",
               h2(style = "color: #1E8CC8;", "Decision Tree"),
               div("The decision tree helps you to identify relevant data quality frameworks for a specific research context", style = "font-size: 14px;")
             ),
             fluidPage(
               fluidRow(
                 column(4,
                        plotOutput("bar_chart"),
                        uiOutput("bar_chart_desc"),
                        uiOutput("paper_count", style = "margin-top: 100px;")
                 ),
                 column(8,
                        helpText("Filter the list using the dropdowns below:"),
                        div(style="border: 1px solid #ccc; padding: 15px; border-radius: 5px;",
                            fluidRow(
                              column(4,
                                     selectInput("col_datatype", "Datatype:", choices = c("","Untargeted", "Social Media Data", "Visual Data", "Survey Data", "Sensor Data", "Register Data")),
                                     actionButton("clear_datatype", "Clear")
                              ),
                              column(4,
                                     selectInput("col_perspective", "Perspective:", choices = c("", "extrinsic", "intrinsic")),
                                     actionButton("clear_perspective", "Clear")
                              ),
                              column(4,
                                     selectInput("col_granularity", "Granularity:", choices = c("", "general", "specific")),
                                     actionButton("clear_granularity", "Clear")
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
             value = "evidence_gap_map",
             div(
               style = "width: 100%; text-align: center; margin-bottom: 20px;",
               h2(style = "color: #1E8CC8;", "Evidence Gap Map"),
               div("The evidence gap map shows which quality dimensions are covered for social science data types.", style = "font-size: 14px;")
             ),
             tags$iframe(
               src = "map_final.html",
               width = "100%",
               height = "1000px",
               style = "border:none; overflow:auto;"
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    dt_data %>%
      filter(
        (input$col_datatype == "" | case_when(
          input$col_datatype == "Untargeted" ~ Untargeted == 1,
          input$col_datatype == "Social Media Data" ~ `Social Media Data` == 1,
          input$col_datatype == "Visual Data" ~ `Visual Data` == 1,
          input$col_datatype == "Survey Data" ~ `Survey Data` == 1,
          input$col_datatype == "Sensor Data" ~ `Sensor Data` == 1,
          input$col_datatype == "Register Data" ~ `Register Data` == 1,
          TRUE ~ TRUE
        )) &
          (input$col_perspective == "" | case_when(
            input$col_perspective == "extrinsic" ~ dummy_extrinsic == 1,
            input$col_perspective == "intrinsic" ~ dummy_intrinsic == 1,
            TRUE ~ TRUE
          )) &
          (input$col_granularity == "" | dummy_granularity == input$col_granularity)
      )
  })

  output$recommendations_table <- renderDataTable({
    filtered_data() %>%
      mutate(meta_doi = ifelse(!is.na(meta_doi) & meta_doi != "",
                               paste0("<a href='https://doi.org/", meta_doi, "' target='_blank'>", meta_doi, "</a>"), NA)) %>%
      select(Title = meta_title, Authors = meta_authors, Link = meta_doi) %>%
      datatable(escape = FALSE, options = list(pageLength = 10))
  })

  output$bar_chart <- renderPlot({
    data_long <- filtered_data() %>%
      select(Untargeted, `Social Media Data`, `Register Data`, `Survey Data`, `Sensor Data`, `Visual Data`, dummy_granularity) %>%
      mutate_all(~replace_na(., 0)) %>%
      pivot_longer(cols = -dummy_granularity, names_to = "datatype", values_to = "value") %>%
      mutate(value = as.numeric(value)) %>%
      filter(value == 1) %>%
      mutate(granularity = factor(ifelse(dummy_granularity == "specific", "Specific", "General"), levels = c("General", "Specific"))) %>%
      count(datatype, granularity)

    data_long$datatype <- factor(data_long$datatype, levels = c("Sensor Data", "Social Media Data", "Register Data", "Untargeted", "Survey Data"))

    if (nrow(data_long) == 0) return(NULL)

    ggplot(data_long, aes(x = datatype, y = n, fill = interaction(datatype, granularity, sep = "."))) +
      geom_bar(stat = "identity", position = "stack") +
      coord_flip() +
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
      theme(legend.position = "none", plot.title = element_text(size = 19))
  })

  output$bar_chart_desc <- renderUI({
    HTML("<p style='text-align: center; font-size: 14px; color: gray;'>Darker colors = Specific; Lighter colors = General granularity</p>")
  })

  output$paper_count <- renderUI({
    count <- nrow(filtered_data())
    div(style = "border: 2px solid black; padding: 20px; border-radius: 10px; text-align: center; width: 200px; margin: auto;",
        div(style = "font-size: 40px; font-weight: bold;", count),
        div(style = "font-size: 14px; color: grey; margin-top: 5px;", "Papers Found"))
  })

  output$download_bibtex <- downloadHandler(
    filename = function() {"references.bib"},
    content = function(file) {
      bibtex_data <- filtered_data() %>%
        select(bibtex) %>%
        mutate(bibtex = paste0("@", bibtex))
      writeLines(bibtex_data$bibtex, file)
    }
  )

  # Internal tab switchers via in-text links
  observe({
    shinyjs::runjs("
      document.getElementById('link_to_tree').onclick = function() {
        Shiny.setInputValue('go_to_tree', Math.random());
      };
      document.getElementById('link_to_map').onclick = function() {
        Shiny.setInputValue('go_to_map', Math.random());
      };
    ")
  })

  observeEvent(input$go_to_tree, {
    updateNavbarPage(session, "main_navbar", selected = "decision_tree")
  })
  observeEvent(input$go_to_map, {
    updateNavbarPage(session, "main_navbar", selected = "evidence_gap_map")
  })

  # Reset buttons
  observeEvent(input$clear_datatype, {
    updateSelectInput(session, "col_datatype", selected = "")
  })
  observeEvent(input$clear_perspective, {
    updateSelectInput(session, "col_perspective", selected = "")
  })
  observeEvent(input$clear_granularity, {
    updateSelectInput(session, "col_granularity", selected = "")
  })
}

shinyApp(ui, server)

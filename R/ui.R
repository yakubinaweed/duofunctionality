library(shiny)
library(bslib)
library(refineR)
library(readxl)
library(moments)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)

ui <- navbarPage(
  title = "RefineR Reference Interval Estimation",
  id = "tabs",
  theme = bs_theme(version = 4, base_font = font_google("Inter"), heading_font = font_google("Rethink Sans"), font_scale = 1.1, bootswatch = "default"),

  tabPanel(
    title = "Main Analysis",
    useShinyjs(),

    tags$head(
      includeCSS("www/styles.css")
    ),

    tags$script(HTML("
      var analysisRunning = false;

      Shiny.addCustomMessageHandler('analysisStatus', function(status) {
        analysisRunning = status;
        // Optionally disable/enable the tab links directly here
        if (status) {
          $('a[data-value=\"Second Tab\"]').attr('data-toggle', 'disabled').addClass('disabled-tab-link');
        } else {
          $('a[data-value=\"Second Tab\"]').attr('data-toggle', 'tab').removeClass('disabled-tab-link');
        }
      });

      $(document).on('click', 'a[data-toggle=\"tab\"]', function(event) {
        if (analysisRunning) {
          event.preventDefault();
          Shiny.setInputValue('tab_switch_blocked', new Date().getTime());
          return false;
        }
      });
    ")),

    sidebarLayout(
      sidebarPanel(
        style = "padding-right: 15px;",

        selectInput(inputId = "gender_choice", label = "Select Gender:",
                    choices = c("Male" = "M", "Female" = "F", "Both" = "Both"), selected = "Both"),

        sliderInput(inputId = "age_range", label = "Age Range:", min = 0, max = 100, value = c(0, 100), step = 1),

        fileInput(inputId = "data_file", label = "Upload Data (Excel File)", accept = c(".xlsx")),

        selectInput(inputId = "col_value", label = "Select Column for Values:",
                    choices = c("None" = ""), selected = ""),

        selectInput(inputId = "col_age", label = "Select Column for Age:",
                    choices = c("None" = ""), selected = ""),

        selectInput(inputId = "col_gender", label = "Select Column for Gender:",
                    choices = c("None" = ""), selected = ""),

        radioButtons(inputId = "nbootstrap_speed",
                     label = "Select Computation Speed:",
                     choices = c("Fast", "Medium", "Slow"),
                     selected = "Fast",
                     inline = TRUE),

        actionButton("analyze_btn", "Analyze", class = "btn-primary"),
        actionButton("reset_btn", "Reset File", class = "btn-secondary"),

        shinyFiles::shinyDirButton(id = "select_dir_btn", label = "Select Output Directory", title = "Select a directory to save plots", style = "margin-top: 5px;"),

        div(style = "margin-top: 5px; display: flex; align-items: center; justify-content: flex-start; width: 100%;",
            prettySwitch(inputId = "enable_directory",
                         label = "Auto-Save Graph",
                         status = "success",
                         fill = TRUE,
                         inline = TRUE)
        ),

        uiOutput("app_message"),
        hr(),

        numericInput("ref_low", "Reference Lower Limit:", value = NA),
        numericInput("ref_high", "Reference Upper Limit:", value = NA),
        textInput(inputId = "unit_input", label = "Unit of Measurement", value = "mmol/L", placeholder = "mmol/L")
      ),

      mainPanel(
        style = "padding-left: 15px;",
        plotOutput("result_plot", height = "400px"),
        verbatimTextOutput("result_text")
      )
    )
  ),

  tabPanel(
    title = "Second Tab",
    useShinyjs(), # Ensure shinyjs is enabled for this tab too if needed
    h4("Gaussian Mixture Model (GMM) Analysis"),
    fluidRow(
      column(4,
             fileInput(inputId = "gmm_data_file", label = "Upload Data (Excel File)", accept = c(".xlsx")),
             selectInput(inputId = "gmm_col_value", label = "Select Column for GMM Analysis:",
                         choices = c("None" = ""), selected = ""),
             # Added for plotting with age
             selectInput(inputId = "gmm_col_age", label = "Select Column for Age (for plotting):",
                         choices = c("None" = ""), selected = ""),
             numericInput(inputId = "gmm_n_components", label = "Number of GMM Components:", value = 2, min = 1),
             actionButton("run_gmm_analysis", "Run GMM Analysis", class = "btn-primary"),
             actionButton("reset_gmm_btn", "Reset GMM Data", class = "btn-secondary")
      ),
      column(8,
             h4("GMM Results Plot"),
             plotOutput("gmm_plot", height = "400px"),
             h4("GMM Summary"),
             verbatimTextOutput("gmm_summary")
      )
    )
    # You can add more fluidRows or elements for other functionalities like RefineR or Z-scoring here
  ),

  footer = tags$footer(
    HTML('© 2025 <a href="https://github.com/yakubinaweed/refineR-reference-interval" target="_blank">Naweed Yakubi</a> • All rights reserved.'),
    style = "
      position: relative;
      bottom: 0;
      width: 100%;
      text-align: center;
      padding: 10px 0;
      color: #777;
      font-size: 0.8em;"
  )
)
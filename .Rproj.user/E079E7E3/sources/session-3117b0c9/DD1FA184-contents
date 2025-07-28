# server.R

# Load necessary libraries (already done in app.R/ui.R but good to have here too for standalone testing)
library(shiny)
library(readxl) # For read_excel
library(shinyjs) # For shinyjs functions

server <- function(input, output, session) {
  # Reactive values for main analysis
  analysis_running <- reactiveVal(FALSE)
  current_tab <- reactiveVal("Main Analysis") # Tracks the currently selected tab
  data_reactive <- reactiveVal(NULL)
  selected_dir_reactive <- reactiveVal(NULL)
  message_rv <- reactiveVal(NULL)

  # Source your main analysis server logic files
  source("server/reactive_values.R", local = TRUE)
  source("server/data_observers.R", local = TRUE)
  source("server/file_observers.R", local = TRUE)
  source("server/analysis_observers.R", local = TRUE)
  source("server/output_renderers.R", local = TRUE)
  source("utils.R", local = TRUE) # Ensure utils.R is sourced if it contains shared functions

  # Source files for the second tab's server logic
  # These files contain functions to be called, not necessarily independent observers
  source("serversecond/gmms.R", local = TRUE)
  source("serversecond/plotting.R", local = TRUE)
  source("serversecond/refiner.R", local = TRUE) # If you have RefineR specific logic for this tab
  source("serversecond/standard_z.R", local = TRUE) # If you have Z-score specific logic for this tab

  # --- Shared/Centralized Logic ---
  # Centralized message display
  render_app_message(output, message_rv)

  # Call your main analysis observer (make sure it sets analysis_running TRUE/FALSE)
  call_analysis_observer(input, output, session, data_reactive, selected_dir_reactive, analysis_running, message_rv)


  # --- Logic for the Second Tab (GMM Analysis Example) ---
  gmm_data_reactive <- reactiveVal(NULL) # Reactive value for data uploaded in Second Tab

  # Observer for file input in the Second Tab
  observeEvent(input$gmm_data_file, {
    req(input$gmm_data_file)
    # Ensure analysis is not running on main tab before processing new data
    if (!analysis_running()) {
      tryCatch({
        data <- read_excel(input$gmm_data_file$datapath)
        gmm_data_reactive(data)
        updateSelectInput(session, "gmm_col_value",
                          choices = c("None" = "", colnames(data)), selected = "")
        message_rv(list(text = "GMM data loaded successfully!", type = "success"))
      }, error = function(e) {
        message_rv(list(text = paste("Error loading GMM data:", e$message), type = "danger"))
      })
    } else {
      message_rv(list(text = "Cannot load data while Main Analysis is running. Please wait or reset.", type = "warning"))
    }
  })

  # Observer for "Run GMM Analysis" button
  observeEvent(input$run_gmm_analysis, {
    # Only proceed if Second Tab is active and no other analysis is running
    if (input$tabs == "Second Tab" && !analysis_running()) {
      req(gmm_data_reactive(), input$gmm_col_value) # Ensure data and column are selected
      if (input$gmm_col_value == "None" || is.null(input$gmm_col_value) || input$gmm_col_value == "") {
        message_rv(list(text = "Please select a column for GMM analysis.", type = "warning"))
        return()
      }

      analysis_running(TRUE) # Indicate GMM analysis is running
      message_rv(list(text = "Running GMM analysis...", type = "info"))

      tryCatch({
        data_for_gmm <- gmm_data_reactive()[[input$gmm_col_value]]
        # Filter out NA values and ensure it's numeric
        data_for_gmm <- as.numeric(na.omit(data_for_gmm))

        if (length(data_for_gmm) < 2) { # Need at least 2 data points for meaningful analysis
            stop("Not enough valid data points in the selected column for GMM analysis.")
        }

        # Call functions from serversecond/gmms.R
        gmm_model_result <- run_gmm(data_for_gmm, input$gmm_n_components)
        clustered_df_result <- assign_clusters(gmm_data_reactive(), gmm_model_result)

        # Render plot using functions from serversecond/plotting.R
        output$gmm_plot <- renderPlot({
          # Adjust plotting function parameters as needed by plot_age_hgb or new GMM-specific plot
          plot_age_hgb(df = clustered_df_result, age_col = input$gmm_col_age, hgb_col = input$gmm_col_value) # Assuming plot_age_hgb can be adapted or you create a new plot function
        })

        # Render summary
        output$gmm_summary <- renderPrint({
          summary(gmm_model_result$model) # Assuming gmm_model_result$model is the actual Mclust object
        })

        message_rv(list(text = "GMM analysis complete!", type = "success"))

      }, error = function(e) {
        message_rv(list(text = paste("Error during GMM analysis:", e$message), type = "danger"))
      }, finally = {
        analysis_running(FALSE) # Analysis finished
      })
    } else if (input$tabs != "Second Tab") {
      message_rv(list(text = "Please switch to the 'Second Tab' to run this analysis.", type = "warning"))
    }
  })

  # Observer for "Reset GMM Data" button
  observeEvent(input$reset_gmm_btn, {
    if (!analysis_running()) { # Only reset if no analysis is running
      gmm_data_reactive(NULL)
      updateSelectInput(session, "gmm_col_value", choices = c("None" = ""), selected = "")
      output$gmm_plot <- renderPlot(plot.new()) # Clear plot
      output$gmm_summary <- renderPrint({ cat("") }) # Clear text
      message_rv(list(text = "GMM data and results reset.", type = "info"))
    } else {
      message_rv(list(text = "Cannot reset GMM data while Main Analysis is running. Please wait or reset Main Analysis first.", type = "warning"))
    }
  })

  # --- Global Tab Switching Logic ---
  observeEvent(input$tabs, {
    # This observeEvent will fire even if the tab switch is blocked client-side,
    # but the analysis_running flag ensures server-side actions are appropriate.
    if (!analysis_running()) {
      current_tab(input$tabs)
      clear_messages(message_rv) # Clear messages when switching tabs

      # Reset elements of the Main Analysis tab when switching away from it
      if (input$tabs != "Main Analysis") {
        data_reactive(NULL)
        selected_dir_reactive(NULL)
        output$result_text <- renderPrint({ cat("") })
        output$result_plot <- renderPlot(plot.new())
        updateSelectInput(session, "col_value", choices = c("None" = ""), selected = "")
        updateSelectInput(session, "col_age", choices = c("None" = ""), selected = "")
        updateSelectInput(session, "col_gender", choices = c("None" = ""), selected = "")
        updateSelectInput(session, "gender_choice", selected = "Both")
        updateSliderInput(session, "age_range", value = c(0, 100))
        updateRadioButtons(session, "nbootstrap_speed", selected = "Fast")
        updateTextInput(session, "unit_input", value = "mmol/L")
        updateNumericInput(session, "ref_low", value = NA)
        updateNumericInput(session, "ref_high", value = NA)
        shinyjs::reset("data_file") # Resets the file input visually
      }

      # Reset elements of the Second Tab when switching away from it
      if (input$tabs != "Second Tab") {
        gmm_data_reactive(NULL)
        updateSelectInput(session, "gmm_col_value", choices = c("None" = ""), selected = "")
        output$gmm_plot <- renderPlot(plot.new())
        output$gmm_summary <- renderPrint({ cat("") })
        shinyjs::reset("gmm_data_file")
      }
    } else {
      # If analysis is running, prevent server-side tab change logic,
      # client-side JS should already block the visual change.
      # You might want to revert the tab if the JS fails or is bypassed
      # For example: updateNavbarPage(session, "tabs", selected = current_tab())
      message_rv(list(text = "Tab switch blocked: An analysis is currently running. Please wait or reset the analysis.", type = "warning"))
    }
  })

  # Observe tab_switch_blocked from client-side JS for server-side feedback
  observeEvent(input$tab_switch_blocked, {
    if (analysis_running()) {
      message_rv(list(text = "Cannot switch tabs while an analysis is running. Please wait or reset the analysis.", type = "warning"))
    }
  })
}
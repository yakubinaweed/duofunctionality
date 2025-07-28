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
  source("serversecond/gmms.R", local = TRUE)
  source("serversecond/plotting.R", local = TRUE)
  source("serversecond/refiner.R", local = TRUE)
  source("serversecond/standard_z.R", local = TRUE)

  # --- Shared/Centralized Logic ---
  # Centralized message display
  render_app_message(output, message_rv)

  # Call your main analysis observer (make sure it sets analysis_running TRUE/FALSE)
  call_analysis_observer(input, output, session, data_reactive, selected_dir_reactive, analysis_running, message_rv)


  # --- Logic for the Second Tab (GMM Analysis Example) ---
  gmm_data_reactive <- reactiveVal(NULL) # Reactive value for data uploaded in Second Tab

  # Observer for file input in the Second Tab
  observeEvent(input$gmm_data_file, {
    print("DEBUG: gmm_data_file observer triggered.")
    req(input$gmm_data_file)
    if (!analysis_running()) {
      tryCatch({
        print("DEBUG: Attempting to read GMM data.")
        data <- read_excel(input$gmm_data_file$datapath)
        gmm_data_reactive(data)
        col_names <- colnames(data)

        updateSelectInput(session, "gmm_col_value",
                          choices = c("None" = "", col_names), selected = "")
        updateSelectInput(session, "gmm_col_age",
                          choices = c("None" = "", col_names), selected = "")
        message_rv(list(text = "GMM data loaded successfully!", type = "success"))
        print("DEBUG: GMM data loaded and selectors updated.")
      }, error = function(e) {
        message_rv(list(text = paste("Error loading GMM data:", e$message), type = "danger"))
        print(paste("DEBUG ERROR: Error loading GMM data:", e$message))
      })
    } else {
      message_rv(list(text = "Cannot load data while Main Analysis is running. Please wait or reset.", type = "warning"))
      print("DEBUG: GMM data load blocked by main analysis.")
    }
  })

  # Observer for "Run GMM Analysis" button
  observeEvent(input$run_gmm_analysis, {
    print("DEBUG: run_gmm_analysis observer triggered.")
    if (input$tabs == "Second Tab" && !analysis_running()) {
      req(gmm_data_reactive(), input$gmm_col_value, input$gmm_col_age)
      if (input$gmm_col_value == "None" || is.null(input$gmm_col_value) || input$gmm_col_value == "" ||
          input$gmm_col_age == "None" || is.null(input$gmm_col_age) || input$gmm_col_age == "") {
        message_rv(list(text = "Please select both 'Column for GMM Analysis' and 'Column for Age (for plotting)'.", type = "warning"))
        print("DEBUG: GMM analysis: Missing column selection.")
        return()
      }

      analysis_running(TRUE)
      message_rv(list(text = "Running GMM analysis...", type = "info"))
      print("DEBUG: GMM analysis started.")

      tryCatch({
        print(paste("DEBUG: Extracting data for GMM from column:", input$gmm_col_value))
        data_for_gmm_vector <- gmm_data_reactive()[[input$gmm_col_value]] # This is a vector
        data_for_gmm_vector <- as.numeric(na.omit(data_for_gmm_vector))
        print(paste("DEBUG: Data for GMM prepared. Length:", length(data_for_gmm_vector)))

        if (length(data_for_gmm_vector) < 2) {
            stop("Not enough valid data points in the selected column for GMM analysis.")
        }

        # Convert the vector to a single-column matrix before passing to run_gmm
        data_for_gmm_matrix <- as.matrix(data_for_gmm_vector)
        print(paste("DEBUG: Data for GMM converted to matrix. Dimensions:", paste(dim(data_for_gmm_matrix), collapse = "x")))


        print(paste("DEBUG: Calling run_gmm with N components:", input$gmm_n_components))
        # Pass the matrix to run_gmm
        gmm_model_result <- run_gmm(data_for_gmm_matrix, G_range = input$gmm_n_components)
        print("DEBUG: run_gmm call completed.")

        print("DEBUG: Calling assign_clusters.")
        clustered_df_result <- assign_clusters(gmm_data_reactive(), gmm_model_result)
        print("DEBUG: assign_clusters call completed.")

        print("DEBUG: Rendering GMM plot.")
        output$gmm_plot <- renderPlot({
          if (!input$gmm_col_age %in% colnames(clustered_df_result) || !input$gmm_col_value %in% colnames(clustered_df_result)) {
             stop("Selected Age or Value column not found in data for plotting.")
          }
          plot_age_hgb(df = clustered_df_result, age = input$gmm_col_age, hgb = input$gmm_col_value)
        })
        print("DEBUG: GMM plot rendering initiated.")

        print("DEBUG: Rendering GMM summary.")
        output$gmm_summary <- renderPrint({
          summary(gmm_model_result$model)
        })
        print("DEBUG: GMM summary rendering initiated.")

        message_rv(list(text = "GMM analysis complete!", type = "success"))
        print("DEBUG: GMM analysis complete message sent.")

      }, error = function(e) {
        message_rv(list(text = paste("Error during GMM analysis:", e$message), type = "danger"))
        print(paste("DEBUG ERROR: Error during GMM analysis:", e$message))
      }, finally = {
        analysis_running(FALSE)
        print("DEBUG: GMM analysis finally block executed.")
      })
    } else if (input$tabs != "Second Tab") {
      message_rv(list(text = "Please switch to the 'Second Tab' to run this analysis.", type = "warning"))
      print("DEBUG: GMM analysis blocked: Not on Second Tab.")
    }
  })

  # Observer for "Reset GMM Data" button
  observeEvent(input$reset_gmm_btn, {
    print("DEBUG: reset_gmm_btn observer triggered.")
    if (!analysis_running()) {
      gmm_data_reactive(NULL)
      updateSelectInput(session, "gmm_col_value", choices = c("None" = ""), selected = "")
      updateSelectInput(session, "gmm_col_age", choices = c("None" = ""), selected = "")
      output$gmm_plot <- renderPlot(plot.new())
      output$gmm_summary <- renderPrint({ cat("") })
      message_rv(list(text = "GMM data and results reset.", type = "info"))
      print("DEBUG: GMM data and results reset.")
    } else {
      message_rv(list(text = "Cannot reset GMM data while Main Analysis is running. Please wait or reset Main Analysis first.", type = "warning"))
      print("DEBUG: GMM reset blocked by main analysis.")
    }
  })

  # --- Global Tab Switching Logic ---
  observeEvent(input$tabs, {
    print(paste("DEBUG: Tab switch observed to:", input$tabs))
    if (!analysis_running()) {
      current_tab(input$tabs)
      clear_messages(message_rv)
      print("DEBUG: Clearing messages on tab switch.")

      if (input$tabs != "Main Analysis") {
        print("DEBUG: Resetting Main Analysis tab elements.")
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
        shinyjs::reset("data_file")
      }

      if (input$tabs != "Second Tab") {
        print("DEBUG: Resetting Second Tab elements.")
        gmm_data_reactive(NULL)
        updateSelectInput(session, "gmm_col_value", choices = c("None" = ""), selected = "")
        updateSelectInput(session, "gmm_col_age", choices = c("None" = ""), selected = "")
        output$gmm_plot <- renderPlot(plot.new())
        output$gmm_summary <- renderPrint({ cat("") })
        shinyjs::reset("gmm_data_file")
      }
    } else {
      message_rv(list(text = "Tab switch blocked: An analysis is currently running. Please wait or reset the analysis.", type = "warning"))
      print("DEBUG: Tab switch blocked by running analysis.")
    }
  })

  observeEvent(input$tab_switch_blocked, {
    if (analysis_running()) {
      message_rv(list(text = "Cannot switch tabs while an analysis is running. Please wait or reset the analysis.", type = "warning"))
      print("DEBUG: Client-side tab switch blocked signal received.")
    }
  })
}
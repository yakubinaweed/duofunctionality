# R/server.R (Complete Script)

library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse) # Includes dplyr, ggplot2, tibble, etc.
library(mclust) # For GMM analysis
library(moments) # For skewness calculation
library(shinyjs) # For UI manipulations

# Source utility R files (ensure these paths are correct relative to server.R)
source("R/serversecond/gmms.R", local = TRUE)
source("R/serversecond/standard_z.R", local = TRUE)
source("R/serversecond/plotting.R", local = TRUE)


# Reactive value for analysis status
analysis_running_rv <- reactiveVal(FALSE)

# Reactive value for messages
message_rv <- reactiveVal(list(text = "Welcome!", type = "info"))


server <- function(input, output, session) {

  # =========================================================================
  # Global Observers and Reactive Values
  # =========================================================================

  # Observer to prevent tab switching while analysis is running
  observeEvent(input$tabs, {
    if (analysis_running_rv()) {
      message_rv(list(text = "Please wait, an analysis is currently running. Tab switching is disabled.", type = "warning"))
      # Revert to the previous tab if an analysis is running
      # This requires storing the last active tab, which is more complex.
      # For now, just display a message and disable the tabs.
    }
  }, ignoreInit = TRUE)

  # Display messages
  output$message_output <- renderUI({
    msg <- message_rv()
    if (!is.null(msg$text)) {
      class_name <- switch(msg$type,
                           "info" = "alert alert-info",
                           "success" = "alert alert-success",
                           "warning" = "alert alert-warning",
                           "error" = "alert alert-danger")
      div(class = class_name, msg$text)
    }
  })

  # =========================================================================
  # Window 1: Main Analysis Tab (refineR) - (Original structure, not fully revised here)
  # =========================================================================

  # Reactive value for uploaded data
  uploaded_data_rv <- reactiveVal(NULL)
  refiner_results_rv <- reactiveVal(NULL) # To store refineR analysis results

  # UI for Main Analysis Tab (moved to a renderUI block for better structure)
  output$main_analysis_tab <- renderUI({
    tagList(
      fluidRow(
        column(4,
               box(title = "1. Upload Data (Excel)", width = NULL,
                   fileInput("file_upload", "Choose Excel File",
                             multiple = FALSE,
                             accept = c(".xlsx", ".xls"))
               ),
               box(title = "2. Select Columns and Filters", width = NULL,
                   uiOutput("value_col_selector"),
                   uiOutput("age_col_selector"),
                   uiOutput("gender_col_selector"),
                   radioButtons("gender_filter", "Filter by Gender:",
                                choices = c("Male", "Female", "Both"),
                                selected = "Both", inline = TRUE),
                   sliderInput("age_range_filter", "Age Range:",
                               min = 0, max = 100, value = c(0, 100)),
                   textInput("unit_input", "Unit of Measurement (e.g., g/dL):", value = "")
               ),
               box(title = "3. Run Analysis", width = NULL,
                   actionButton("run_analysis_btn", "Analyze Data", class = "btn-primary"),
                   actionButton("reset_file_btn", "Reset File", class = "btn-danger"),
                   hr(),
                   h4("Graph Options"),
                   textInput("lower_limit_input", "User-Defined Lower Limit:", value = ""),
                   textInput("upper_limit_input", "User-Defined Upper Limit:", value = ""),
                   checkboxInput("auto_save_graph", "Auto-Save Graph", value = FALSE),
                   conditionalPanel(
                     condition = "input.auto_save_graph == true",
                     actionButton("select_output_dir", "Select Output Directory", class = "btn-info")
                   )
               )
        ),
        column(8,
               tabBox(
                 title = "Analysis Results",
                 id = "refiner_results_tabs", width = NULL,
                 tabPanel("Plot", plotOutput("plot_output", height = "500px")),
                 tabPanel("Summary", verbatimTextOutput("summary_output"))
               )
        )
      )
    )
  })


  # Observers for file upload and column selection for Main Analysis tab
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      data <- readxl::read_excel(input$file_upload$datapath)
      uploaded_data_rv(data)
      message_rv(list(text = "Data uploaded successfully.", type = "success"))
    }, error = function(e) {
      message_rv(list(text = paste("Error reading file:", e$message), type = "error"))
      uploaded_data_rv(NULL)
    })
  })

  output$value_col_selector <- renderUI({
    data <- uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("value_col", "Select Value Column:", choices = names(data),
                selected = c("HGB", "hgb", "Value", "value")[c("HGB", "hgb", "Value", "value") %in% names(data)][1])
  })

  output$age_col_selector <- renderUI({
    data <- uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("age_col", "Select Age Column:", choices = names(data),
                selected = c("Age", "age")[c("Age", "age") %in% names(data)][1])
  })

  output$gender_col_selector <- renderUI({
    data <- uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gender_col", "Select Gender Column:", choices = names(data),
                selected = c("Gender", "gender", "Sex", "sex")[c("Gender", "gender", "Sex", "sex") %in% names(data)][1])
  })

  # Observer for analysis button
  observeEvent(input$run_analysis_btn, {
    req(uploaded_data_rv(), input$value_col, input$age_col, input$gender_col, input$gender_filter, input$age_range_filter)

    if (analysis_running_rv()) {
      message_rv(list(text = "An analysis is already running. Please wait.", type = "warning"))
      return(NULL)
    }

    analysis_running_rv(TRUE)
    shinyjs::disable("refiner_results_tabs") # Disable tab switching

    withProgress(message = 'Running RefineR Analysis', value = 0, {
      incProgress(0.1, detail = "Preparing data...")

      data <- uploaded_data_rv()
      value_col <- input$value_col
      age_col <- input$age_col
      gender_col <- input$gender_col
      gender_filter_val <- input$gender_filter
      age_range <- input$age_range_filter
      unit_of_measurement <- input$unit_input

      # Data filtering (simplified, assume helper functions exist)
      filtered_data <- data %>%
        dplyr::select(Value = !!sym(value_col), Age = !!sym(age_col), Gender = !!sym(gender_col)) %>%
        na.omit() %>%
        filter(Age >= age_range[1] & Age <= age_range[2])

      if (gender_filter_val != "Both") {
        filtered_data <- filtered_data %>%
          filter(str_detect(Gender, regex(gender_filter_val, ignore_case = TRUE)))
      }

      if (nrow(filtered_data) == 0) {
        message_rv(list(text = "No data remains after filtering. Adjust filters or check data.", type = "warning"))
        refiner_results_rv(NULL)
        analysis_running_rv(FALSE)
        shinyjs::enable("refiner_results_tabs")
        return(NULL)
      }

      incProgress(0.4, detail = "Running refineR algorithm...")
      # Placeholder for refineR analysis
      # In a real app, you'd call refineR functions here
      # For example:
      # library(refineR)
      # ref_model <- refineR(data = filtered_data$Value, ...)
      # ref_intervals <- extract_intervals(ref_model) # Assuming such functions exist

      # Mock refineR results for demonstration
      mock_intervals <- list(
        lower = quantile(filtered_data$Value, 0.025, na.rm = TRUE),
        upper = quantile(filtered_data$Value, 0.975, na.rm = TRUE)
      )
      refiner_results_rv(list(
        data = filtered_data,
        intervals = mock_intervals,
        user_lower = as.numeric(input$lower_limit_input),
        user_upper = as.numeric(input$upper_limit_input),
        unit = unit_of_measurement
      ))

      incProgress(0.4, detail = "Generating plot and summary...")
      message_rv(list(text = "RefineR analysis complete!", type = "success"))
    }) # End of withProgress

    analysis_running_rv(FALSE)
    shinyjs::enable("refiner_results_tabs") # Re-enable tab switching
  })

  # Observer for resetting file
  observeEvent(input$reset_file_btn, {
    uploaded_data_rv(NULL)
    refiner_results_rv(NULL)
    shinyjs::reset("file_upload") # Reset file input
    shinyjs::reset("gender_filter")
    shinyjs::reset("age_range_filter")
    shinyjs::reset("unit_input")
    shinyjs::reset("lower_limit_input")
    shinyjs::reset("upper_limit_input")
    shinyjs::reset("auto_save_graph")
    message_rv(list(text = "File and analysis results reset.", type = "info"))
  })

  # Render Plot for Main Analysis Tab
  output$plot_output <- renderPlot({
    results <- refiner_results_rv()
    if (is.null(results)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Upload data and run analysis to see plot.", size = 6, color = "grey50"))
    }

    plot_refiner_output(results$data,
                        results$intervals$lower, results$intervals$upper,
                        results$user_lower, results$user_upper,
                        results$unit)
  })

  # Render Summary for Main Analysis Tab
  output$summary_output <- renderPrint({
    results <- refiner_results_rv()
    if (is.null(results)) {
      return("No analysis results to display.")
    }

    cat("RefineR Analysis Results:\n")
    cat(paste0("Estimated Lower Limit (2.5th percentile): ", round(results$intervals$lower, 2), " ", results$unit, "\n"))
    cat(paste0("Estimated Upper Limit (97.5th percentile): ", round(results$intervals$upper, 2), " ", results$unit, "\n"))

    if (!is.na(results$user_lower) || !is.na(results$user_upper)) {
      cat("\nUser-Defined Limits:\n")
      if (!is.na(results$user_lower)) {
        cat(paste0("User Lower Limit: ", results$user_lower, " ", results$unit, "\n"))
      }
      if (!is.na(results$user_upper)) {
        cat(paste0("User Upper Limit: ", results$user_upper, " ", results$unit, "\n"))
      }
    }
  })

  # Auto-save graph logic (placeholder for directory selection)
  output_dir_rv <- reactiveVal(NULL)

  observeEvent(input$select_output_dir, {
    # This is a placeholder. In a real desktop app, you'd use a file chooser.
    # For Shiny web app, user can only choose a directory on the server.
    # For local development, rstudioapi::selectDirectory() could be used.
    # For deployment, you'd typically have a pre-configured output path.
    # For simplicity, let's assume a 'downloads' subfolder or similar.
    selected_dir <- file.path(getwd(), "downloads") # Example: current working directory + /downloads
    dir.create(selected_dir, showWarnings = FALSE) # Create if not exists
    output_dir_rv(selected_dir)
    message_rv(list(text = paste("Output directory set to:", selected_dir), type = "info"))
  })

  observeEvent(input$run_analysis_btn, {
    if (input$auto_save_graph && !is.null(refiner_results_rv()) && !is.null(output_dir_rv())) {
      plot_obj <- plot_refiner_output(refiner_results_rv()$data,
                                      refiner_results_rv()$intervals$lower, refiner_results_rv()$intervals$upper,
                                      refiner_results_rv()$user_lower, refiner_results_rv()$user_upper,
                                      refiner_results_rv()$unit)

      filename <- generate_safe_filename("refiner_plot", "png")
      filepath <- file.path(output_dir_rv(), filename)
      ggsave(filepath, plot = plot_obj, width = 10, height = 6, units = "in")
      message_rv(list(text = paste("Graph saved to:", filepath), type = "success"))
    }
  }, ignoreInit = TRUE)


  # =========================================================================
  # Window 2: Subpopulation Detection (GMM) - REVISED
  # =========================================================================

  # Reactive value to store GMM data (uploaded and processed)
  gmm_uploaded_data_rv <- reactiveVal(NULL)
  gmm_processed_data_rv <- reactiveVal(NULL)
  gmm_transformation_details_rv <- reactiveVal(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))


  # UI for GMM Tab
  output$gmm_tab <- renderUI({
    tagList(
      fluidRow(
        column(4,
               box(title = "1. Upload Data (Excel)", width = NULL,
                   fileInput("gmm_file_upload", "Choose Excel File",
                             multiple = FALSE,
                             accept = c(".xlsx", ".xls"))
               ),
               box(title = "2. Select Columns", width = NULL,
                   uiOutput("gmm_hgb_col_selector"),
                   uiOutput("gmm_age_col_selector"),
                   uiOutput("gmm_gender_col_selector") # Gender selector added
               ),
               box(title = "3. Run Analysis", width = NULL,
                   actionButton("run_gmm_analysis_btn", "Run Subpopulation Detection", class = "btn-primary"),
                   actionButton("reset_gmm_analysis_btn", "Reset GMM Data", class = "btn-danger")
               )
        ),
        column(8,
               tabBox(
                 title = "Subpopulation Detection Results",
                 id = "gmm_results_tabs", width = NULL,
                 tabPanel("GMM Plot", plotOutput("plot_output_gmm", height = "500px")),
                 tabPanel("GMM Summary", verbatimTextOutput("gmm_summary_output")),
                 tabPanel("Cluster Age Group Summary", tableOutput("gmm_age_group_summary_output"))
               )
        )
      )
    )
  })

  # Observers for file upload and column selection for GMM tab
  observeEvent(input$gmm_file_upload, {
    req(input$gmm_file_upload)
    tryCatch({
      data <- readxl::read_excel(input$gmm_file_upload$datapath)
      gmm_uploaded_data_rv(data)
      message_rv(list(text = "GMM data uploaded successfully.", type = "success"))
    }, error = function(e) {
      message_rv(list(text = paste("Error reading GMM file:", e$message), type = "error"))
      gmm_uploaded_data_rv(NULL)
    })
  })

  output$gmm_hgb_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_hgb_col", "Select HGB Column:", choices = names(data),
                selected = c("HGB", "hgb", "HB", "hb")[c("HGB", "hgb", "HB", "hb") %in% names(data)][1])
  })

  output$gmm_age_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_age_col", "Select Age Column:", choices = names(data),
                selected = c("Age", "age")[c("Age", "age") %in% names(data)][1])
  })

  output$gmm_gender_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_gender_col", "Select Gender Column:", choices = names(data),
                selected = c("Gender", "gender", "Sex", "sex")[c("Gender", "gender", "Sex", "sex") %in% names(data)][1])
  })


  # Observer for GMM analysis button - REVISED LOGIC
  observeEvent(input$run_gmm_analysis_btn, {
    req(gmm_uploaded_data_rv(), input$gmm_hgb_col, input$gmm_age_col, input$gmm_gender_col)

    if (analysis_running_rv()) {
      message_rv(list(text = "An analysis is already running. Please wait.", type = "warning"))
      return(NULL)
    }

    # Set analysis running flag and block tab switching
    analysis_running_rv(TRUE)
    shinyjs::disable("gmm_results_tabs") # Disable tab switching during analysis

    # Show progress modal
    withProgress(message = 'Running GMM Analysis', value = 0, {
      incProgress(0.1, detail = "Loading data...")

      data <- gmm_uploaded_data_rv()
      hgb_col <- input$gmm_hgb_col
      age_col <- input$gmm_age_col
      gender_col <- input$gmm_gender_col

      if (!all(c(hgb_col, age_col, gender_col) %in% names(data))) {
        message_rv(list(text = "Selected columns not found in data. Please check selections.", type = "error"))
        analysis_running_rv(FALSE)
        shinyjs::enable("gmm_results_tabs")
        return(NULL)
      }

      # Prepare data for GMM
      gmm_data <- data %>%
        dplyr::select(HGB = !!sym(hgb_col), Age = !!sym(age_col), Gender_orig = !!sym(gender_col)) %>% # Keep original gender name
        mutate(original_row_index = row_number()) %>% # Keep original index for rejoining if needed
        na.omit() # Remove rows with any NA in selected columns

      if (nrow(gmm_data) == 0) {
        message_rv(list(text = "No complete rows for GMM after NA removal. Check data or selections.", type = "error"))
        analysis_running_rv(FALSE)
        shinyjs::enable("gmm_results_tabs")
        return(NULL)
      }

      incProgress(0.2, detail = "Splitting data by gender and transforming...")

      # Normalize gender column to "Male" and "Female"
      gmm_data <- gmm_data %>%
        mutate(Gender = case_when(
          str_detect(Gender_orig, regex("male|m", ignore_case = TRUE)) ~ "Male",
          str_detect(Gender_orig, regex("female|f", ignore_case = TRUE)) ~ "Female",
          TRUE ~ "Other" # Label other values as 'Other'
        )) %>%
        filter(Gender %in% c("Male", "Female")) # Only proceed with Male/Female for this analysis

      male_data <- gmm_data %>% filter(Gender == "Male")
      female_data <- gmm_data %>% filter(Gender == "Female")

      combined_clustered_data <- tibble()
      male_hgb_transformed_flag <- FALSE
      female_hgb_transformed_flag <- FALSE

      # Process Male Data
      if (nrow(male_data) > 0) {
        message_rv(list(text = paste0("Processing ", nrow(male_data), " male records."), type = "info"))
        # Apply conditional Yen-Johnson to HGB for males
        # Note: apply_conditional_yen_johnson is a simplified log transform. For full YJ, car::powerTransform is better.
        yj_result_male <- apply_conditional_yen_johnson(male_data$HGB)
        male_data$HGB_transformed <- yj_result_male$transformed_data
        male_hgb_transformed_flag <- yj_result_male$transformation_applied

        # Z-standardize HGB (transformed) and Age for males
        male_data$HGB_z <- z_transform(male_data$HGB_transformed)
        male_data$Age_z <- z_transform(male_data$Age)

        incProgress(0.2, detail = "Running GMM for Male data...")
        tryCatch({
          male_gmm_model <- run_gmm(male_data %>% dplyr::select(HGB = HGB_z, Age = Age_z))
          male_data <- assign_clusters(male_data, male_gmm_model)
          # Re-assign cluster to the combined data (important for plotting original values)
          male_data$cluster <- as.factor(male_data$cluster)
          message_rv(list(text = "GMM for male data complete.", type = "success"))
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for male data:", e$message), type = "error"))
        })
        # Select relevant columns and add to combined
        # Keep original HGB for plotting and summary, but cluster is from transformed data
        combined_clustered_data <- bind_rows(combined_clustered_data,
                                             male_data %>% dplyr::select(HGB, Age, Gender, cluster))
      } else {
        message_rv(list(text = "No male data to process.", type = "warning"))
      }


      # Process Female Data
      if (nrow(female_data) > 0) {
        message_rv(list(text = paste0("Processing ", nrow(female_data), " female records."), type = "info"))
        # Apply conditional Yen-Johnson to HGB for females
        yj_result_female <- apply_conditional_yen_johnson(female_data$HGB)
        female_data$HGB_transformed <- yj_result_female$transformed_data
        female_hgb_transformed_flag <- yj_result_female$transformation_applied

        # Z-standardize HGB (transformed) and Age for females
        female_data$HGB_z <- z_transform(female_data$HGB_transformed)
        female_data$Age_z <- z_transform(female_data$Age)

        incProgress(0.2, detail = "Running GMM for Female data...")
        tryCatch({
          female_gmm_model <- run_gmm(female_data %>% dplyr::select(HGB = HGB_z, Age = Age_z))
          female_data <- assign_clusters(female_data, female_gmm_model)
          # Re-assign cluster to the combined data (important for plotting original values)
          female_data$cluster <- as.factor(female_data$cluster)
          message_rv(list(text = "GMM for female data complete.", type = "success"))
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for female data:", e$message), type = "error"))
        })
        # Select relevant columns and add to combined
        # Keep original HGB for plotting and summary, but cluster is from transformed data
        combined_clustered_data <- bind_rows(combined_clustered_data,
                                             female_data %>% dplyr::select(HGB, Age, Gender, cluster))
      } else {
        message_rv(list(text = "No female data to process.", type = "warning"))
      }

      # Store transformation details
      gmm_transformation_details_rv(list(male_hgb_transformed = male_hgb_transformed_flag,
                                         female_hgb_transformed = female_hgb_transformed_flag))

      if (nrow(combined_clustered_data) > 0) {
        gmm_processed_data_rv(combined_clustered_data)
        message_rv(list(text = "GMM analysis complete!", type = "success"))
      } else {
        message_rv(list(text = "No data available after GMM processing for plotting/summary.", type = "error"))
        gmm_processed_data_rv(NULL)
      }

      incProgress(0.1, detail = "Generating plots and summaries...")
    }) # End of withProgress

    analysis_running_rv(FALSE)
    shinyjs::enable("gmm_results_tabs") # Re-enable tab switching

  }) # End of observeEvent for run_gmm_analysis_btn


  # Observer for resetting GMM tab
  observeEvent(input$reset_gmm_analysis_btn, {
    gmm_uploaded_data_rv(NULL)
    gmm_processed_data_rv(NULL)
    gmm_transformation_details_rv(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))
    # Reset UI elements visually
    shinyjs::reset("gmm_file_upload")
    message_rv(list(text = "GMM data and results reset.", type = "info"))
  })


  # Render GMM Plot
  output$plot_output_gmm <- renderPlot({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No GMM data available for plotting.", size = 6, color = "grey50"))
    }

    # Pass transformation flags to the plotting function for title/notes
    plot_age_hgb(plot_data,
                 male_hgb_transformed = gmm_transformation_details_rv()$male_hgb_transformed,
                 female_hgb_transformed = gmm_transformation_details_rv()$female_hgb_transformed)
  })


  # Render GMM Summary (updated to handle gender-split summary)
  output$gmm_summary_output <- renderPrint({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return("No GMM analysis results to display.")
    }

    # Summarize for Male data
    male_summary <- plot_data %>%
      filter(Gender == "Male") %>%
      group_by(cluster) %>%
      summarise(
        Proportion = n() / nrow(.) * 100,
        Mean_HGB = mean(HGB, na.rm = TRUE),
        SD_HGB = sd(HGB, na.rm = TRUE),
        Mean_Age = mean(Age, na.rm = TRUE),
        SD_Age = sd(Age, na.rm = TRUE),
        Min_Age = min(Age, na.rm = TRUE),
        Max_Age = max(Age, na.rm = TRUE)
      ) %>%
      mutate_if(is.numeric, ~round(., 2))

    # Summarize for Female data
    female_summary <- plot_data %>%
      filter(Gender == "Female") %>%
      group_by(cluster) %>%
      summarise(
        Proportion = n() / nrow(.) * 100,
        Mean_HGB = mean(HGB, na.rm = TRUE),
        SD_HGB = sd(HGB, na.rm = TRUE),
        Mean_Age = mean(Age, na.rm = TRUE),
        SD_Age = sd(Age, na.rm = TRUE),
        Min_Age = min(Age, na.rm = TRUE),
        Max_Age = max(Age, na.rm = TRUE)
      ) %>%
      mutate_if(is.numeric, ~round(., 2))

    cat("--- GMM Analysis Summary (Male Subpopulations) ---\n")
    if (nrow(male_summary) > 0) {
      print(male_summary)
    } else {
      cat("No male subpopulations detected.\n")
    }

    cat("\n--- GMM Analysis Summary (Female Subpopulations) ---\n")
    if (nrow(female_summary) > 0) {
      print(female_summary)
    } else {
      cat("No female subpopulations detected.\n")
    }

    # Add a note about transformation if applied
    if (gmm_transformation_details_rv()$male_hgb_transformed || gmm_transformation_details_rv()$female_hgb_transformed) {
      cat("\nNote: HGB values were transformed (log) for GMM input due to skewness. Reported HGB values are original.\n")
    }
  })


  # Render GMM Cluster Age Group Summary (updated for gender)
  output$gmm_age_group_summary_output <- renderTable({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    # Predefined age bins for reporting (ensure these are consistent with any other use)
    age_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf)
    age_labels <- c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

    plot_data %>%
      mutate(age_group_label = cut(Age, breaks = age_bins, labels = age_labels, right = FALSE)) %>%
      group_by(Gender, age_group_label, cluster) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = cluster, values_from = Count, values_fill = 0)
  }, rownames = FALSE)

}
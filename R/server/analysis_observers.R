# R/server/analysis_observers.R

# Helper function to perform all input and data validation // AI-generated
perform_validation <- function(input, data, col_value, col_age, col_gender, enable_directory, selected_dir_reactive, message_rv) {
  # 1. Check if data file is uploaded
  if (is.null(data)) {
    message_rv(list(type = "error", text = "Error: Please upload an Excel file before analyzing."))
    return(FALSE)
  }

  # 2. Check if the REQUIRED Value column has been selected
  if (is.null(col_value) || col_value == "") {
    message_rv(list(type = "error", text = "Error: Please select a column for Values."))
    return(FALSE)
  }
  # Age and Gender columns are now optional, so no need to check if they are "" or NULL here.
  # The filtering logic will handle the "None" option.

  # 3. Directory check if auto-save is enabled but no directory is selected
  if (enable_directory && is.null(selected_dir_reactive())) {
    message_rv(list(type = "error", text = "Error: Auto-save is enabled, but no directory is selected. Please select a directory."))
    return(FALSE)
  }

  # If all checks pass
  clear_messages(message_rv)
  return(TRUE)
}


# This function encapsulates the main analysis logic triggered by the analyze_btn 
call_analysis_observer <- function(input, output, session, data_reactive, selected_dir_reactive, analysis_running) {
  observeEvent(input$analyze_btn, {
    # Indicate analysis started
    analysis_running(TRUE)
    
    # Disable button and update UI
    shinyjs::disable("analyze_btn")
    shinyjs::html("analyze_btn", "Analyzing...")
    message_rv(list(type = "info", text = "Analysis in progress..."))

    # Define variables from inputs (needed for validation and analysis)
    col_value <- input$col_value
    col_age <- input$col_age
    col_gender <- input$col_gender

    # --- Perform Validation ---
    if (!perform_validation(input, data_reactive(), col_value, col_age, col_gender,
                          input$enable_directory, selected_dir_reactive, message_rv)) {
      analysis_running(FALSE)  # Reset flag here on validation failure
      shinyjs::enable("analyze_btn")
      shinyjs::html("analyze_btn", "Analyze")
      return()
    }

    # If validation passed, get the clean data
    data <- data_reactive()
    units <- input$unit_input
    gender_choice <- input$gender_choice
    age_range <- input$age_range

    # --- Data Filtering and Preparation ---
    gender_map <- list(
      "M" = c("M", "Male", "male", "Man", "man", "Jongen", "jongen"),
      "F" = c("F", "V", "Female", "female", "Woman", "woman", "Meisje", "meisje", "Vrouw", "vrouw")
    )

    processed_data <- data

    if (col_gender != "" && gender_choice %in% c("M", "F")) {
      processed_data <- subset(processed_data, processed_data[[col_gender]] %in% gender_map[[gender_choice]])
    }

    if (col_age != "") {
      processed_data <- subset(processed_data, processed_data[[col_age]] >= age_range[1] & processed_data[[col_age]] <= age_range[2])
    }

    numeric_data <- as.numeric(na.omit(processed_data[[col_value]]))

    # Ensure enough data points
    if (length(numeric_data) < 10) {
      display_analysis_error(output, message_rv, paste0("Error: Not enough data points (", length(numeric_data), ") remaining after filtering for analysis. Please adjust your filters or upload more data."))
      analysis_running(FALSE)  # Reset flag here
      shinyjs::enable("analyze_btn")
      shinyjs::html("analyze_btn", "Analyze")
      return()
    }

    # --- RefineR Model Selection ---
    skewness_value <- moments::skewness(numeric_data)
    min_data <- min(numeric_data)

    chosen_model <- if (min_data > 0) {
      if (abs(skewness_value) <= 1) {
        "BoxCox"
      } else {
        "modBoxCox"
      }
    } else {
      "modBoxCox"
    }

    nbootstrap_value <- switch(input$nbootstrap_speed,
                              "Fast" = 1,
                              "Medium" = 100,
                              "Slow" = 200)

    # --- Perform Reference Interval Estimation ---
    result <- tryCatch({
      refineR::findRI(Data = numeric_data, model = chosen_model, NBootstrap = nbootstrap_value, seed = 123)
    }, error = function(e) {
      display_analysis_error(output, message_rv, paste("Error during reference interval estimation:", e$message))
      analysis_running(FALSE)  # Reset flag on error
      return(NULL)
    })

    if (is.null(result)) {
      analysis_running(FALSE)  # Reset flag if analysis failed
      shinyjs::enable("analyze_btn")
      shinyjs::html("analyze_btn", "Analyze")
      return()
    }

    # --- Display Results ---
    render_results_text(output, result)

    plot_title_parts <- c()
    if (col_gender != "") {
      plot_title_parts <- c(plot_title_parts, ifelse(gender_choice == "Both", "M/F", gender_choice))
    }
    if (col_age != "") {
      plot_title_parts <- c(plot_title_parts, sprintf("A%d-%d", age_range[1], age_range[2]))
    }
    plot_title_prefix <- ifelse(length(plot_title_parts) > 0, paste(plot_title_parts, collapse = " "), "All Subjects")
    plot_title <- sprintf("%s Estimated Reference Interval [%s]", plot_title_prefix, units)

    render_results_plot(output, result, input, plot_title, col_value, units)

    # --- Auto-Save Plot ---
    if (input$enable_directory) { 
      save_plot_to_file(result, input, selected_dir_reactive(), plot_title, col_value, units)
      message_rv(list(type = "success", text = "Analysis complete and graph saved!"))
    } else {
      message_rv(list(type = "success", text = "Analysis complete!"))
      print("Auto-save is disabled. Skipping file generation.")
    }

    # Re-enable analyze button and reset text
    analysis_running(FALSE)  # Reset flag at the end
    shinyjs::enable("analyze_btn")
    shinyjs::html("analyze_btn", "Analyze")
  })
}
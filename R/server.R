server <- function(input, output, session) {
  analysis_running <- reactiveVal(FALSE)
  current_tab <- reactiveVal("Main Analysis")  # default tab name, adjust if needed
  
  # Source your reactive values, observers, renderers etc.
  source("server/reactive_values.R", local = TRUE)
  source("server/data_observers.R", local = TRUE)
  source("server/file_observers.R", local = TRUE)
  source("server/analysis_observers.R", local = TRUE)
  source("server/output_renderers.R", local = TRUE)
  
  # Set up centralized message display
  render_app_message(output, message_rv)
  
  # Call your main analysis observer (make sure it sets analysis_running TRUE/FALSE)
  call_analysis_observer(input, output, session, data_reactive, selected_dir_reactive, analysis_running)
  
  # Computation speed observer (optional)
  observeEvent(input$nbootstrap_speed, {
    print(paste("Selected Computation Speed:", input$nbootstrap_speed))
  })
  
  observe({
    session$sendCustomMessage('analysisStatus', analysis_running())
  })

  observeEvent(input$tab_switch_blocked, {
    showNotification("Cannot switch tabs while analysis is running.", type = "error")
  })
  
 observeEvent(input$tabs, {
    # Only update current_tab() when allowed
    if (!analysis_running()) {
      current_tab(input$tabs)
      # Your resets...
      data_reactive(NULL)
      selected_dir_reactive(NULL)
      clear_messages(message_rv)

      output$result_text <- renderPrint({ cat("") })
      output$result_plot <- renderPlot(plot.new())
      output$app_message <- renderUI(NULL)

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
  })
}

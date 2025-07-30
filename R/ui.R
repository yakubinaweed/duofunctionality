library(shiny)
library(bslib)
library(refineR)
library(readxl)
library(moments)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(shinydashboard) # Added for box/tabBox usage (though it's in server.R, good to have here too if UI uses directly)

ui <- navbarPage(
  title = "RefineR Reference Interval Estimation",
  id = "tabs",
  theme = bs_theme(version = 4, base_font = font_google("Inter"), heading_font = font_google("Rethink Sans"), font_scale = 1.1, bootswatch = "default"),

  tabPanel(
    title = "Main Analysis",
    # This tabPanel will be populated by renderUI in server.R
    uiOutput("main_analysis_tab")
  ),

  tabPanel(
    title = "Subpopulation Detection (GMM)", # Updated title
    # This tabPanel will be populated by renderUI in server.R
    uiOutput("gmm_tab")
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
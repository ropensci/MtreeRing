#' @importFrom shiny actionButton checkboxInput div fileInput conditionalPanel
#' downloadButton downloadHandler helpText icon numericInput observeEvent
#' plotOutput reactiveValues renderPlot renderTable selectInput shinyApp
#' sliderInput tableOutput tabPanel textInput updateActionButton brushOpts
#' dblclickOpts column shinyAppDir
#' updateCheckboxInput updateTextInput hr br fluidRow
#' @importFrom shinydashboard dashboardHeader dashboardSidebar dashboardBody
#' dashboardPage sidebarMenu tabBox tabItem tabItems menuItem box
#' @importFrom  shinyWidgets prettySwitch prettyCheckbox prettyRadioButtons
#' radioGroupButtons sendSweetAlert updatePrettySwitch updatePrettyCheckbox
#' updatePrettyRadioButtons useSweetAlert colorSelectorInput
#' @importFrom utils write.csv
#' @importFrom dplR write.rwl
#' @title Run Shiny-based Application
#' @description Run a Shiny-based application within the system's default 
#' web browser.
#' @param launch.browser A logical value. If \code{TRUE}, the system's default 
#' web browser will be launched automatically after the app is started. 
#' @details 
#' A workflow for the Shiny app can be found in the README file of this 
#' package. Here is a link \url{https://github.com/JingningShi/MtreeRing}
#' 
#' To stop the app, press the Escape key, or click the stop sign icon 
#' in the upper right corner of the RStudio console.
#' @author Jingning Shi, Wei Xiang
#' @export


ring_app_launch <- function(launch.browser = TRUE) {
  app_dir <- system.file('mtr_app', package = 'MtreeRing')
  shinyAppDir(app_dir, options = list(launch.browser = launch.browser))
}

#' @title Run Shiny-based Application
#' @description Run a Shiny-based application within the system's default 
#' web browser.
#' @details 
#' A workflow for the Shiny app can be found in the README file of this 
#' package. Here is a link \url{https://github.com/JingningShi/MtreeRing}
#' @author Jingning Shi, Wei Xiang
#' @export


launchMtRApp <- function() {
  shinyApp(createUI(), createServer, options = list(launch.browser = T))
}

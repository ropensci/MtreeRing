launchMtRApp <- function() {
  shinyApp(createUI(), createServer, options = list(launch.browser = T))
}

#' Launch UBcovidsurv Shiny App
#'
#'@param ... additional arguments to be passed to the \link[shiny]{runApp} function.
#'
#'@examples
#'if(interactive()){
#' UBcovidsurv::run_app()
#'}
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(...) {
  shiny::runApp(app = shinyApp(
    ui = app_ui, 
    server = app_server
  ))
}


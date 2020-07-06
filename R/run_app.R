#' Run the "dragon" Shiny Application
#'
#' @examples
#' \dontrun{
#' library(dragon)
#' dragon::run_app()
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function() {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list() #list(...) I have no args
  )
}


#' Run the "dragon" Shiny Application. Wrapper for dragon::run_app(). 
#'
#' @examples
#' \dontrun{
#' library(dragon)
#' dragon::run_dragon()
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_dragon <- function() {
  run_app()
}

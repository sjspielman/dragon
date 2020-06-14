#' Run the "dragon" Shiny Application
#'
#' \dontrun{
#' library(dragon)
#' dragon::run_app() # No arguments necessary! 
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}

#' Run the dragon Shiny application locally
#'
#' @export
rundragon <- function() {
  appDir <- system.file("dragonapp", package = "dragon")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

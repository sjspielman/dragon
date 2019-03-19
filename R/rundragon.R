#' Run the dragon Shiny application locally
#'
#' @export
rundragon <- function() {
  appDir <- system.file("dragon", package = "dragon")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `dragon` from github with `remotes::install_github('spielmanlab/dragon').", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

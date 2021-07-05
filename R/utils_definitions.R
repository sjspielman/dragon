## Options ---------------------------------------------------------------------------
# syntax source: https://r-pkgs.org/r.html 
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dragon <- list(htmlwidgets.TOJSON_ARGS = list(na = 'string'), ## Setting for DT to show NA cells as NA rather than blank 
                    scipen = 3)
  toset <- !(names(op.dragon) %in% names(op))
  if(any(toset)) options(op.dragon[toset])
}


## Future planning with RStudio check to avoid warning -------------------------------
# Note 2 (yes, 2): Recommended to remove by {future} authors in issues 43/44
# Note 1: syntax below is as issue #43
#if (future::supportsMulticore()) {
#  future::plan(future::multicore)
#} else {
#  future::plan(future::multisession)
#}

## MED URLs and similar --------------------------------------------------------------------------

#' Lead portion of MED URL 
#' @noRd
med_exporting_url <- "https://rruff.info/mineral_list/MED/exporting/"

#' URL of `tbl_mineral.csv` data from MED
#' @noRd
med_m1_url        <- paste0(med_exporting_url, "tbl_mineral.csv")

#' URL of `tbl_locality_age_cache_alt.csv` data from MED
#' @noRd
med_m2_url        <- paste0(med_exporting_url, "tbl_locality_age_cache_alt.csv")

#' URL of dragon github to file issues, used in error messages
#' @noRd
dragon_github_issue_url <- "https://github.com/spielmanlab/dragon/issues"

## Explicit pipe definitions ---------------------------------------------------------
#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @keywords internal
#' @noRd
#' @importFrom magrittr %>%
NULL

#' Pipe assignment operator
#'
#' @name %>%
#' @keywords internal
#' @noRd
#' @importFrom magrittr %<>%
NULL

#' Promises pipe operator
#'
#' @name %...>%
#' @keywords internal
#' @noRd
#' @importFrom promises %...>%
NULL


#' Enjoyable error messages for randomized use in sweetAlerts
#' @noRd
error_choices <- c("Oh no!", "Sorry, that's not gonna work.", "Try again!", "Womp womp :(", "No dice!", "Uh oh!", "Woopsies!")


#' Tibble of element metadata
#' @noRd



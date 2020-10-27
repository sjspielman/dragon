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


## Enjoyable error messages ----------------------------------------------------------------
#' Enjoyable error messages for randomized use in sweetAlerts
#' @noRd
error_choices <- c("Oh no!", "Sorry, that's not gonna work.", "Try again!", "Womp womp :(", "No dice!", "Uh oh!", "Woopsies!")



#' Tibble of element metadata
#' @noRd
element_info <- tibble::tribble(
  ~element_name, ~element, ~element_hsab, ~atomic_mass, ~number_of_protons, ~element_table_period, ~element_table_group, ~atomic_radius, ~pauling, ~element_metal_type, ~element_density, ~element_specific_heat,
  "Silver", "Ag", "Soft acid", 107.868, 47, 5, 11, 1.80, 1.93, "Transition Metal", 1.05e+01, 0.235, 
  "Aluminum", "Al", "Hard acid", 26.982, 13, 3, 13, 1.80, 1.61, "Metal", 2.70e+00, 0.897, 
  "Arsenic", "As", "Hard acid", 74.922, 33, 4, 15, 1.30, 2.18, "Metalloid", 5.78e+00, 0.329, 
  "Gold", "Au", "Soft acid", 196.967, 79, 6, 11, 1.80, 2.54, "Transition Metal", 1.93e+01, 0.129, 
  "Boron", "B", "Soft acid", 10.811, 5, 2, 13, 1.20, 2.04, "Metalloid", 2.34e+00, 1.026, 
  "Barium", "Ba", "Hard acid", 137.327, 56, 6, 2, 2.80, 0.89, "Alkaline Earth Metal", 3.59e+00, 0.204, 
  "Beryllium", "Be", "Hard acid", 9.012, 4, 2, 2, 1.40, 1.57, "Alkaline Earth Metal", 1.85e+00, 1.825, 
  "Bismuth", "Bi", "Int. acid", 208.980, 83, 6, 15, 1.60, 2.02, "Metal", 9.81e+00, 0.122, 
  "Bromine", "Br", "Soft base", 79.904, 35, 4, 17, 1.10, 2.96, "Halogen", 3.12e+00, 0.474, 
  "Carbon", "C", "Soft base", 12.011, 6, 2, 14, 0.91, 2.55, "Nonmetal", 2.27e+00, 0.709, 
  "Calcium", "Ca", "Hard acid", 40.078, 20, 4, 2, 2.20, 1.00, "Alkaline Earth Metal", 1.54e+00, 0.647, 
  "Cadmium", "Cd", "Soft acid", 112.411, 48, 5, 12, 1.70, 1.69, "Transition Metal", 8.69e+00, 0.232, 
  "Cerium", "Ce", "Hard acid", 140.116, 58, 6, NA, 2.70, 1.12, "Lanthanide", 6.77e+00, 0.192, 
  "Chlorine", "Cl", "Int. base", 35.453, 17, 3, 17, 0.97, 3.16, "Halogen", 3.21e-03, 0.479, 
  "Cobalt", "Co", "Int. acid", 58.933, 27, 4, 9, 1.70, 1.88, "Transition Metal", 8.86e+00, 0.421, 
  "Chromium", "Cr", "Hard acid", 51.996, 24, 4, 6, 1.90, 1.66, "Transition Metal", 7.15e+00, 0.449, 
  "Cesium", "Cs", "Hard acid", 132.905, 55, 6, 1, 3.30, 0.79, "Alkali Metal", 1.87e+00, 0.242, 
  "Copper", "Cu", "Int. acid", 63.546, 29, 4, 11, 1.60, 1.90, "Transition Metal", 8.96e+00, 0.385, 
  "Dysprosium", "Dy", "Hard acid", 162.500, 66, 6, NA, 2.50, 1.22, "Lanthanide", 8.55e+00, 0.170, 
  "Erbium", "Er", "Hard acid", 167.259, 68, 6, NA, 2.50, 1.24, "Lanthanide", 9.07e+00, 0.168, 
  "Fluorine", "F", "Hard base", 18.998, 9, 2, 17, 0.57, 3.98, "Halogen", 1.70e-03, 0.824, 
  "Iron", "Fe", "Int. acid", 55.845, 26, 4, 8, 1.70, 1.83, "Transition Metal", 7.87e+00, 0.449, 
  "Gallium", "Ga", "Hard acid", 69.723, 31, 4, 13, 1.80, 1.81, "Metal", 5.91e+00, 0.371, 
  "Gadolinium", "Gd", "Hard acid", 157.250, 64, 6, NA, 2.50, 1.20, "Lanthanide", 7.90e+00, 0.236, 
  "Germanium", "Ge", "Hard acid", 72.640, 32, 4, 14, 1.50, 2.01, "Metalloid", 5.32e+00, 0.320, 
  "Hydrogen", "H", "Hard acid", 1.007, 1, 1, 1, 0.79, 2.20, "Nonmetal", 8.99e-05, 14.304, 
  "Hafnium", "Hf", "Hard acid", 178.490, 72, 6, 4, 2.20, 1.30, "Transition Metal", 1.33e+01, 0.144, 
  "Mercury", "Hg", "Soft acid", 200.590, 80, 6, 12, 1.80, 2.00, "Transition Metal", 1.35e+01, 0.140, 
  "Iodine", "I", "Soft base", 126.904, 53, 5, 17, 1.30, 2.66, "Halogen", 4.93e+00, 0.214, 
  "Indium", "In", "Int. acid", 114.818, 49, 5, 13, 2.00, 1.78, "Metal", 7.31e+00, 0.233, 
  "Iridium", "Ir", "Soft acid", 192.217, 77, 6, 9, 1.90, 2.20, "Transition Metal", 2.26e+01, 0.131, 
  "Potassium", "K", "Hard acid", 39.098, 19, 4, 1, 2.80, 0.82, "Alkali Metal", 8.62e-01, 0.757, 
  "Lanthanum", "La", "Hard acid", 138.905, 57, 6, 3, 2.70, 1.10, "Lanthanide", 6.15e+00, 0.195, 
  "Lithium", "Li", "Hard acid", 6.941, 3, 2, 1, 2.10, 0.98, "Alkali Metal", 5.34e-01, 3.582, 
  "Magnesium", "Mg", "Hard acid", 24.305, 12, 3, 2, 1.70, 1.31, "Alkaline Earth Metal", 1.74e+00, 1.023, 
  "Manganese", "Mn", "Int. acid", 54.938, 25, 4, 7, 1.80, 1.55, "Transition Metal", 7.44e+00, 0.479, 
  "Molybdenum", "Mo", "Int. acid", 95.960, 42, 5, 6, 2.00, 2.16, "Transition Metal", 1.02e+01, 0.251, 
  "Nitrogen", "N", "Int. base", 14.007, 7, 2, 15, 0.75, 3.04, "Nonmetal", 1.25e-03, 1.040, 
  "Sodium", "Na", "Hard acid", 22.990, 11, 3, 1, 2.20, 0.93, "Alkali Metal", 9.71e-01, 1.228, 
  "Niobium", "Nb", "Hard acid", 92.906, 41, 5, 5, 2.10, 1.60, "Transition Metal", 8.57e+00, 0.265, 
  "Neodymium", "Nd", "Hard acid", 144.242, 60, 6, NA, 2.60, 1.14, "Lanthanide", 7.01e+00, 0.190, 
  "Nickel", "Ni", "Int. acid", 58.693, 28, 4, 10, 1.60, 1.91, "Transition Metal", 8.91e+00, 0.444, 
  "Oxygen", "O", "Hard base", 15.999, 8, 2, 16, 0.65, 3.44, "Nonmetal", 1.43e-03, 0.918, 
  "Osmium", "Os", "Soft acid", 190.230, 76, 6, 8, 1.90, 2.20, "Transition Metal", 2.26e+01, 0.130, 
  "Phosphorus", "P", "Soft base", 30.974, 15, 3, 15, 1.20, 2.19, "Nonmetal", 1.82e+00, 0.769, 
  "Lead", "Pb", "Int. acid", 207.200, 82, 6, 14, 1.80, 2.33, "Metal", 1.13e+01, 0.129, 
  "Palladium", "Pd", "Soft acid", 106.420, 46, 5, 10, 1.80, 2.20, "Transition Metal", 1.20e+01, 0.244, 
  "Platinum", "Pt", "Soft acid", 195.084, 78, 6, 10, 1.80, 2.28, "Transition Metal", 2.15e+01, 0.133, 
  "Rubidium", "Rb", "Hard acid", 85.468, 37, 5, 1, 3.00, 0.82, "Alkali Metal", 1.53e+00, 0.363, 
  "Rhenium", "Re", "Int. acid", 186.207, 75, 6, 7, 2.00, 1.90, "Transition Metal", 2.10e+01, 0.137, 
  "Rare-earth element", "REE", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
  "Rhodium", "Rh", "Soft acid", 102.906, 45, 5, 9, 1.80, 2.28, "Transition Metal", 1.24e+01, 0.243, 
  "Ruthenium", "Ru", "Soft acid", 101.070, 44, 5, 8, 1.90, 2.20, "Transition Metal", 1.24e+01, 0.238, 
  "Sulfur", "S", "Int. base", 32.065, 16, 3, 16, 1.10, 2.58, "Nonmetal", 2.07e+00, 0.710, 
  "Antimony", "Sb", "Hard acid", 121.760, 51, 5, 15, 1.50, 2.05, "Metalloid", 6.69e+00, 0.207, 
  "Scandium", "Sc", "Hard acid", 44.956, 21, 4, 3, 2.10, 1.36, "Transition Metal", 2.99e+00, 0.568, 
  "Selenium", "Se", "Soft base", 78.960, 34, 4, 16, 1.20, 2.55, "Nonmetal", 4.81e+00, 0.321, 
  "Silicon", "Si", "Hard acid", 28.086, 14, 3, 14, 1.50, 1.90, "Metalloid", 2.33e+00, 0.705, 
  "Samarium", "Sm", "Hard acid", 150.360, 62, 6, NA, 2.60, 1.17, "Lanthanide", 7.52e+00, 0.197, 
  "Tin", "Sn", "Hard acid", 118.710, 50, 5, 14, 1.70, 1.96, "Metal", 7.29e+00, 0.228, 
  "Strontium", "Sr", "Hard acid", 87.620, 38, 5, 2, 2.50, 0.95, "Alkaline Earth Metal", 2.64e+00, 0.301, 
  "Tantalum", "Ta", "Int. acid", 180.948, 73, 6, 5, 2.10, 1.50, "Transition Metal", 1.67e+01, 0.140, 
  "Technetium", "Tc", "Int. acid", 98.000, 43, 5, 7, 2.00, 1.90, "Transition Metal", 1.15e+01, NA, 
  "Tellurium", "Te", "Soft base", 127.600, 52, 5, 16, 1.40, 2.10, "Metalloid", 6.23e+00, 0.202, 
  "Thorium", "Th", "Hard acid", 232.038, 90, 7, NA, NA, 1.30, "Actinide", 1.17e+01, 0.113, 
  "Titanium", "Ti", "Hard acid", 47.867, 22, 4, 4, 2.00, 1.54, "Transition Metal", 4.54e+00, 0.523, 
  "Thallium", "Tl", "Soft acid", 204.383, 81, 6, 13, 2.10, 2.04, "Metal", 1.19e+01, 0.129, 
  "Uranium", "U", "Hard acid", 238.029, 92, 7, NA, NA, 1.38, "Actinide", 1.90e+01, 0.116, 
  "Vanadium", "V", "Hard acid", 50.942, 23, 4, 5, 1.90, 1.63, "Transition Metal", 6.11e+00, 0.489, 
  "Tungsten", "W", "Int. acid", 183.840, 74, 6, 6, 2.00, 2.36, "Transition Metal", 1.93e+01, 0.132, 
  "Yttrium", "Y", "Hard acid", 88.906, 39, 5, 3, 2.30, 1.22, "Transition Metal", 4.47e+00, 0.298, 
  "Ytterbium", "Yb", "Hard acid", 173.054, 70, 6, NA, 2.40, 1.10, "Lanthanide", 6.97e+00, 0.155, 
  "Zinc", "Zn", "Int. acid", 65.380, 30, 4, 12, 1.50, 1.65, "Transition Metal", 7.13e+00, 0.388, 
  "Zirconium", "Zr", "Hard acid", 91.224, 40, 5, 4, 2.20, 1.33, "Transition Metal", 6.51e+00, 0.278)




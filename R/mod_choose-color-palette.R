#' UI function for Shiny module to query users for either a single color, or a sequential/diverging color scheme
#' 
#' @param id UI element identifier
#' @param by_label UI element label
#' @param by_choices UI element list of choices for variables to color by
#' @param by_default_selection Default selected choice for `by_choices`
#' @param color_default Default color to use for a single color selection
#' @param palette_default Default RColorBrewer palette to use for a palette selection
#' @noRd
mod_ui_choose_color_sd_palette <- function(id, by_label, by_choices, by_default_selection, color_default, palette_default)
{
  ns <- NS(id)
  tagList(
    fluidRow(
      column(7,
        shinyWidgets::pickerInput(ns("color_by"), by_label, by_choices, selected = by_default_selection),
        style='padding:0px;'
      ),
      column(5,
        conditionalPanel(condition = "input.color_by == 'singlecolor'", ns = ns, 
        {
          colourpicker::colourInput(ns("color"), "Color:", value = color_default)
        }),
        conditionalPanel(condition = "input.color_by != 'singlecolor'", ns = ns, 
        {
          pickerInput(ns("palette"), 
                      label = "Palette:",
                      choices = sd_palettes_ui$name,
                      options = list( size = 6 ),
                      choicesOpt = list(content = sd_palettes_ui$img),
                      selected = palette_default)
        }), style='padding-left:0px;' 
      ) ## END column 5
    ) ## END fluidRow
  ) ## END tagList
}


#' Server function for Shiny module to query users for either a single color, or a sequential/diverging color scheme
#' 
#' @return Reactive list with "color_by", "color", "palette" for use in shiny app
#' @noRd
# mod_server_choose_color_sd_palette <- function(input, output, session) { 
#   reactive({
#     list(color_by = input$color_by,
#          color    = input$color,
#          palette  = input$palette
#         )
#   })
# }
mod_server_choose_color_sd_palette <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        list(color_by = input$color_by,
             color    = input$color,
             palette  = input$palette
        )
      })
    }
  ) 
}

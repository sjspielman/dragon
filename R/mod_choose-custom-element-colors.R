#' UI function for Shiny module to add more custom element colors
#' 
#' @param id UI element identifier
#' @param available Elements to choose from
#' @noRd
mod_ui_choose_custom_element_colors <- function(id, available) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6, 
        shinyWidgets::pickerInput(ns("custom_elements"), 
                                  "Element(s):",             
                                  choices = available,
                                  options = list(`actions-box` = TRUE, size = 6), 
                                  multiple = TRUE
        ), style='padding:0px;'
      ),
      column(6, 
             colourpicker::colourInput(ns("custom_color"), "Color:"), style='padding-left:0px;'
      )
    )
  )
}
#' Server function for Shiny module to add more custom element colors
#' 
#' @return Named list of ("element" = "color")
#' @noRd
mod_server_choose_custom_element_colors <- function(id)
{
  moduleServer(id, function(input, output, session){
      reactive({
        el <- input$custom_elements
        cols <- input$custom_color
        final <- rep(cols, length(el))
        names(final) <- el
        final ## NAME is node and VALUE is color
      }) 
    } # function
  ) #moduleServer
}


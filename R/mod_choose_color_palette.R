#' Shiny module UI portion to choose a sequential/diverging palette
mod_ui_choose_sd_palette <- function(id, default_selection, label = "Palette:") {
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(ns("palette"), label = label,
                choices = palette_sd[["palette_names"]], selected = default_selection, width = "90%",
                choicesOpt = list(
                  content = sprintf(
                    "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                    unname(palette_sd[["linear_gradient"]]), palette_sd[["label_colors"]], names(palette_sd[["linear_gradient"]])
                  )
                )
    )  ## END pickerInput
  ) ## END tagList
}


#' Shiny module server portion to choose a qualitative
mod_server_choose_sd_palette <- function(input, output, session) { 
  reactive({input$palette})
}

#' Shiny module UI portion to choose a sequential/diverging palette
mod_ui_choose_q_palette <- function(id, default_selection, label = "Palette:") {
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(ns("palette"), label = label,
                              choices = palette_q[["palette_names"]], selected = default_selection, width = "90%",
                              choicesOpt = list(
                                content = sprintf(
                                  "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                                  unname(palette_q[["linear_gradient"]]), palette_q[["label_colors"]], names(palette_q[["linear_gradient"]])
                                )
                              )
    )  ## END pickerInput
  ) ## END tagList
}


#' Shiny module server portion to choose a qualitative palette
mod_server_choose_q_palette <- function(input, output, session) { 
  reactive({input$palette})
}




#' Shiny module UI portion to choose a single color
mod_ui_choose_single_color <- function(id, default_selection, label = "Color:") {
  ns <- NS(id)
  tagList(
    colourpicker::colourInput(ns("color"), label, value = default_selection)
  ) ## END tagList
}


#' Shiny module server portion to choose a single color
mod_server_choose_single_color <- function(input, output, session) { 
  reactive({input$color})
}



#' Shiny module UI portion to choose a node shape
mod_ui_choose_shape <- function(id, choices, default_selection, label) {
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(ns("shape"), label, choices, selected = default_selection)
  ) ## END tagList
}



#' Shiny module server portion to choose a node shape
mod_server_choose_shape <- function(input, output, session) { 
  reactive({input$shape})
} 



#' Shiny module UI portion to choose a variable to style by
mod_ui_choose_style_by <- function(id, choices, default_selection, label) {
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(ns("style"), label, choices, selected = default_selection)
  ) ## END tagList
}



#' Shiny module server portion to choose a variable to style by
mod_server_choose_style_by <- function(input, output, session) { 
  reactive({input$style})
} 

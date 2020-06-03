mod_ui_choose_color_sd_palette <- function(id, by_label, by_choices, by_default_selection, color_default, palette_default)
{
  ns <- NS(id)
  tagList(
    fluidRow(
      column(7,
        shinyWidgets::pickerInput(ns("color_by"), by_label, by_choices, selected = by_default_selection)
      ),
      column(5,
        conditionalPanel(condition = "input.color_by == 'singlecolor'", ns = ns, 
        {
          colourpicker::colourInput(ns("color"), "Color:", value = color_default)
        }),
        conditionalPanel(condition = "input.color_by != 'singlecolor'", ns = ns, 
        {
          shinyWidgets::pickerInput(ns("palette"), label = "Palette:",
                choices = palette_sd[["palette_names"]], selected = palette_default, width = "90%",
                choicesOpt = list(
                  content = sprintf(
                    "<div style='width:100%%;border-radius:4px;background:%s;color:%s;font-weight:400;'>%s</div>",
                    unname(palette_sd[["linear_gradient"]]), palette_sd[["label_colors"]], names(palette_sd[["linear_gradient"]])
                  )
                )
          )
        }) 
      ) ## END column 5
    ) ## END fluidRow
  ) ## END tagList
}


mod_server_choose_color_sd_palette <- function(input, output, session) { 
  reactive({
    list(color_by = input$color_by,
         color    = input$color,
         palette  = input$palette
        )
  })
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






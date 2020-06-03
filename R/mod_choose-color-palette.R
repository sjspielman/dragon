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
          pickerInput(ns("palette"), 
                      label = "Palette:",
                      choices = sd_palettes_ui$name,
                      options = list( size = 6 ),
                      choicesOpt = list(content = sd_palettes_ui$img)
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

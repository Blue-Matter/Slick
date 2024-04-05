#' Zigzag_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Zigzag_plot_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' Zigzag_plot Server Functions
#'
#' @noRd
mod_Zigzag_plot_server <- function(id, i18n, Slick_Object, Filter_Selected, parent_session=session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Zigzag_plot_ui("Zigzag_plot_1")

## To be copied in the server
# mod_Zigzag_plot_server("Zigzag_plot_1")

#' Boxplot_OM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Boxplot_OM_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' Boxplot_OM Server Functions
#'
#' @noRd
mod_Boxplot_OM_server <- function(id, i18n, filtered_slick,
                                  nOM, nMP, nPM, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Boxplot_OM_ui("Boxplot_OM_1")

## To be copied in the server
# mod_Boxplot_OM_server("Boxplot_OM_1")

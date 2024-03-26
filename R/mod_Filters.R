#' Filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Filters_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' Filters Server Functions
#'
#' @noRd
mod_Filters_server <- function(id, i18n){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Filters_ui("Filters_1")

## To be copied in the server
# mod_Filters_server("Filters_1")

#' MP_Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MP_Info_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('MPinfo'))
  )
}



#' MP_Info Server Functions
#'
#' @noRd
mod_MP_Info_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$MPinfo <- DT::renderDataTable({
      i18n <- i18n()
      slick <- Slick_Object()
      Table(MPs(slick), i18n$get_translation_language())
    })
  })
}

## To be copied in the UI
# mod_MP_Info_ui("MP_Info_1")

## To be copied in the server
# mod_MP_Info_server("MP_Info_1")

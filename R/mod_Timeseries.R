#' Timeseries UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Timeseries_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Timeseries Server Functions
#'
#' @noRd 
mod_Timeseries_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Timeseries_ui("Timeseries_1")
    
## To be copied in the server
# mod_Timeseries_server("Timeseries_1")

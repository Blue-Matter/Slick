#' Kobe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Kobe_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Kobe Server Functions
#'
#' @noRd 
mod_Kobe_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Kobe_ui("Kobe_1")
    
## To be copied in the server
# mod_Kobe_server("Kobe_1")

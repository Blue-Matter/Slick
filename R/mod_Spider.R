#' Spider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Spider_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Spider Server Functions
#'
#' @noRd 
mod_Spider_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Spider_ui("Spider_1")
    
## To be copied in the server
# mod_Spider_server("Spider_1")

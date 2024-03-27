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

      df <- data.frame(
        Label=Label(MPs(slick), i18n$get_translation_language()),
        Description=Description(MPs(slick), i18n$get_translation_language())
      )
      # links <- Link(MPs(slick))
      # if (length(links)>0)
      #   df$Link <- links

      DT::datatable(df,
                    extensions = 'Responsive',
                    selection='none',
                    options = list(dom = 't',
                                   pageLength=100,
                                   ordering=F))

    })
  })
}

## To be copied in the UI
# mod_MP_Info_ui("MP_Info_1")

## To be copied in the server
# mod_MP_Info_server("MP_Info_1")

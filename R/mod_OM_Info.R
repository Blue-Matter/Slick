
#' OM_Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_OM_Info_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('OMinfo'))
  )
}

#' OM_Info Server Functions
#'
#' @noRd
mod_OM_Info_server <- function(id, i18n, Slick_Object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$OM_factors <- DT::renderDataTable({
      Table(OMs(Slick_Object()), i18n()$get_translation_language(), 'factor')
    })

    output$OM_design <- DT::renderDataTable({
      Table(OMs(Slick_Object()), i18n()$get_translation_language(), 'design')

    })

    output$OMinfo <- renderUI({
      i18n <- i18n()
      tabsetPanel(type="tabs",
                  tabPanel(i18n$t('Factors'),
                           br(),
                           DT::dataTableOutput(ns('OM_factors'))),
                  tabPanel(i18n$t('Design'),
                           br(),
                           DT::dataTableOutput(ns('OM_design')))
      )
    })
  })
}



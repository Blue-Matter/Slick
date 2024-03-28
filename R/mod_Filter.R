#' Filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('filters'))

  )
}

#' Filter Server Functions
#'
#' @noRd
mod_Filter_server <- function(id, i18n, Slick_Object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Selected_OMs <- mod_Filter_OM_server("Filter_OM_1", i18n, Slick_Object)

    Filter_Selected <- reactiveValues()
    observeEvent(Selected_OMs$OMs, {
      Filter_Selected$OMs <- Selected_OMs$OMs
    })

    output$filters <- renderUI({
      i18n <- i18n()
      tagList(
        tabsetPanel(id=ns('filtertabs'),
          tabPanel(i18n$t('Operating Models'),
                   mod_Filter_OM_ui(ns("Filter_OM_1"))
          ),
          tabPanel(i18n$t('Management Procedures'),
                   p('test')
          ),
          tabPanel(i18n$t('Performance Indicators'),
                   p('tst')
          )
        )
      )
    })

    Filter_Selected
  })
}

## To be copied in the UI
# mod_Filter_ui("Filter_1")

## To be copied in the server
# mod_Filter_server("Filter_1")

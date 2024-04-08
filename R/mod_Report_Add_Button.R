#' Report_Add_Button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Report_Add_Button_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('button'))
  )
}

#' Report_Add_Button Server Functions
#'
#' @noRd
mod_Report_Add_Button_server <- function(id, i18n){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$button <- renderUI({
      i18n <- i18n()
      tagList(
        shinyWidgets::actionBttn(ns('add_to_report'),
                                 label=i18n$t('Add to Report'),
                                 icon('pen'),
                                 color='default',size='sm')
      )
    })

    button_pushed <- reactive({
      input$add_to_report
    })

    button_pushed
  })
}

## To be copied in the UI
# mod_Report_Add_Button_ui("Report_Add_Button_1")

## To be copied in the server
# mod_Report_Add_Button_server("Report_Add_Button_1")

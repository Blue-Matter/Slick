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
    br(),
    uiOutput(ns('filters')),
    br(),
    uiOutput(ns('filter_button'))
  )
}


#' Filter Server Functions
#'
#' @noRd
mod_Filter_server <- function(id, i18n, Slick_Object, slot, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Selected_OMs <- mod_Filter_OM_server("Filter_OM_1", i18n, Slick_Object)
    Selected_MPs <- mod_Filter_MP_server("Filter_MP_1", i18n, Slick_Object)
    Selected_PMs <- mod_Filter_PM_server("Filter_PM_1", i18n, Slick_Object, slot)

    Filter_Selected <- reactiveValues()

    observeEvent(Selected_OMs$OMs, {
      shinyjs::show("FilterButton")
    })

    observeEvent(Selected_MPs$MPs, {
      shinyjs::show("FilterButton")
    })

    observeEvent(Selected_PMs$PMs, {
      shinyjs::show("FilterButton")
    })

    output$filters <- renderUI({
      i18n <- i18n()
      tagList(
        tabsetPanel(id=ns('filtertabs'),
          tabPanel(i18n$t('Operating Models'),
                   mod_Filter_OM_ui(ns("Filter_OM_1"))
          ),
          tabPanel(i18n$t('Management Procedures'),
                   mod_Filter_MP_ui(ns("Filter_MP_1"))
          ),
          tabPanel(i18n$t('Performance Indicators'),
                   mod_Filter_PM_ui(ns("Filter_PM_1"))
          )
        )
      )
    })

    output$filter_button <- renderUI({
      i18n <- i18n()
      shinyjs::hidden(shinyWidgets::actionBttn(ns("FilterButton"),
                                               label=i18n$t("FILTER"),
                                               icon("filter", verify_fa=FALSE),
                                               color='danger',size='sm')
      )
    })

    observeEvent(input$FilterButton, {
      shinyjs::hide("FilterButton")
      Filter_Selected$OMs <- Selected_OMs$OMs
      Filter_Selected$MPs <- Selected_MPs$MPs
      Filter_Selected$PMs <- Selected_PMs$PMs
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

    # run once when new Slick_Object loaded
    observe({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        shinyjs::hide("FilterButton")
        Filter_Selected$OMs <- isolate(Selected_OMs$OMs)
        Filter_Selected$MPs <- isolate(Selected_MPs$MPs)
        Filter_Selected$PMs <- isolate(Selected_PMs$PMs)
      }
    })

    Filter_Selected
  })
}

## To be copied in the UI
# mod_Filter_ui("Filter_1")

## To be copied in the server
# mod_Filter_server("Filter_1")

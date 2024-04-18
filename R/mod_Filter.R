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
mod_Filter_server <- function(id, i18n, Slick_Object, slot, parent_session,
                              incOM=TRUE, incMP=TRUE, incPM=TRUE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Selected_OMs <- mod_Filter_select_server("Filter_OM_1", i18n, Slick_Object, 'OMs', incOM)
    Selected_MPs <- mod_Filter_select_server("Filter_MP_1", i18n, Slick_Object, 'MPs', incMP)
    Selected_PMs <- mod_Filter_select_server("Filter_PM_1", i18n, Slick_Object, slot, incPM)

    Filter_Selected <- reactiveValues()

    observeEvent(Selected_OMs$selected, {
      shinyjs::show("FilterButton")
    }, ignoreInit = TRUE)

    observeEvent(Selected_MPs$selected, {
      shinyjs::show("FilterButton")
    }, ignoreInit = TRUE)

    observeEvent(Selected_PMs$selected, {
      shinyjs::show("FilterButton")
    }, ignoreInit = TRUE)


    output$filters <- renderUI({
      i18n <- i18n()
      tagList(
        tabsetPanel(id=ns('filtertabs'),
          tabPanel(i18n$t('Operating Models'),
                   mod_Filter_select_ui(ns("Filter_OM_1"))
          ),
          tabPanel(i18n$t('Management Procedures'),
                   mod_Filter_select_ui(ns("Filter_MP_1"))
          ),
          tabPanel(i18n$t('Performance Indicators'),
                   mod_Filter_select_ui(ns("Filter_PM_1"))
          )
        )
      )
    })

    output$filter_button <- renderUI({
      i18n <- i18n()
      shinyjs::hidden(shinyWidgets::actionBttn(ns("FilterButton"),
                                               label=i18n$t("Apply Filters"),
                                               icon("filter", verify_fa=FALSE),
                                               color='danger',size='sm',
                                               block=T, style="fill")
      )
    })

    observeEvent(input$FilterButton, {
      shinyjs::hide("FilterButton")
      Filter_Selected$OMs <- Selected_OMs$selected
      Filter_Selected$MPs <- Selected_MPs$selected
      Filter_Selected$PMs <- Selected_PMs$selected
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })


    # run once when new Slick_Object loaded
    observe({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        shinyjs::hide("FilterButton")
        Filter_Selected$OMs <- isolate(Selected_OMs$selected)
        Filter_Selected$MPs <- isolate(Selected_MPs$selected)
        Filter_Selected$PMs <- isolate(Selected_PMs$selected)
      }
    })

    Filter_Selected
  })
}

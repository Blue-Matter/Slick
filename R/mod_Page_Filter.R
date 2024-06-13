#' Page_Filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Page_Filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('filters'))
  )
}

#' Page_Filter Server Functions
#'
#' @noRd
mod_Page_Filter_server <- function(id, i18n, Slick_Object, slot, minPM=3, incPM=TRUE,
                                   incIcons=TRUE, icon='circle',
                                   button_description='OM and PI Filters',
                                   home_session=NULL){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    selected_mps <- mod_filter_selection_server("filter_mp", i18n, Slick_Object, slot='MPs',
                                                minN=1, incIcons=incIcons,
                                                icon=icon,
                                                home_session=home_session)
    selected_oms <- mod_filter_selection_om_server("filter_om", i18n, Slick_Object)

    selected_pms <- mod_filter_selection_server("filter_pm", i18n, Slick_Object,
                                                slot=slot, minN=minPM, incPM,
                                                home_session=home_session)


    output$filters <- renderUI({
      i18n <- i18n()
      tagList(
        h4(strong(i18n$t('Filters'))),
        # helper2(h4(strong(i18n$t('Filters')))),
        shinyjs::hidden(shinyWidgets::actionBttn(ns("FilterButton"),
                                                 label=i18n$t("Apply Filters"),
                                                 icon("filter", verify_fa=FALSE),
                                                 color='danger',size='sm',
                                                 block=T, style="fill")
        ),
        div(align = 'left', class='multicol',
            mod_filter_selection_ui(ns("filter_mp")),
            shinyBS::bsCollapse(
              shinyBS::bsCollapsePanel(button_description,
                                       mod_filter_selection_om_ui(ns("filter_om")),
                                       mod_filter_selection_ui(ns("filter_pm"))


              )
            )
        )
      )

    })


    # detect first change
    initial_object <- reactiveValues(initial=TRUE)

    observeEvent(Slick_Object(), {
      initial()
      initial_object$initial <- TRUE
    })

    initial <- reactive({
      Slick_Object()
      oms <- selected_oms()
      mps <- selected_mps()
      pms <- selected_pms()
      if (!incPM)
        pms <- FALSE
      any(c(all(is.null(oms)), all(is.null(mps)), all(is.null(pms))))
    })

    observeEvent(initial(), {
      tt <- isolate(initial())
      if (!tt) {
        initial_object$initial <- FALSE
      }
    })

    observeEvent(initial_object$initial, {
      tt <- isolate(initial())
      if (!tt) {
        Filter_Selected$OMs <- isolate(selected_oms())
        Filter_Selected$MPs <- isolate(selected_mps())
        Filter_Selected$PMs <- isolate(selected_pms())
        shinyjs::delay(30, shinyjs::hide("FilterButton"))
      }
    }, ignoreInit = TRUE)

    observeEvent(selected_mps(), {
      shinyjs::show("FilterButton")
    }, ignoreInit = TRUE)

    observeEvent(selected_oms(), {
      shinyjs::show("FilterButton")
    }, ignoreInit = TRUE)

    observeEvent(selected_pms(), {
      shinyjs::show("FilterButton")
    }, ignoreInit = TRUE)

    observeEvent(Slick_Object(), {
      shinyjs::delay(600,
                     shinyjs::click("FilterButton"))
    })

    Filter_Selected <- reactiveValues()

    observeEvent(input$FilterButton, {
      shinyjs::hide("FilterButton")
      Filter_Selected$OMs <- selected_oms()
      Filter_Selected$MPs <- selected_mps()
      Filter_Selected$PMs <- selected_pms()

    })
    Filter_Selected
  })
}

## To be copied in the UI
# mod_Page_Filter_ui("Page_Filter_1")

## To be copied in the server
# mod_Page_Filter_server("Page_Filter_1",i18n,Slick_Object, slot)

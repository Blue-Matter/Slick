selectedMPs <- function(mp_obj) {
  defaults <- Default(mp_obj)
  if (length(defaults)<1) {
    out <- 1:length(Label(mp_obj))
  } else {
    out <- defaults
  }
  out
}

filterMPs <- function(mp_obj, Filter_Selected, input) {
  keep <- rep(TRUE, length(Label(mp_obj)))
  keep <- (1:length(Label(mp_obj)))  %in% input$Filter_MP
  if (sum(keep)==0) {
    # select all if none are selected
    shinyjs::click('reset_button')
  } else {
    Filter_Selected$MPs <- keep
  }
}

#' Filter_MP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Filter_MP_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns('filters')),
    br(),
    fluidRow(
      column(3, uiOutput(ns('defaults'))),
      column(3, uiOutput(ns('filter_button')))
    )
  )
}

#' Filter_MP Server Functions
#'
#' @noRd
mod_Filter_MP_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected <- reactiveValues()

    MP_object <- reactive({
      MPs(Slick_Object())
    })

    output$filters <- renderUI({
      i18n <- i18n()
      labels <- Label(MP_object(), i18n$get_translation_language())

      out <- checkboxGroupInput(ns('Filter_MP'),
                                label='',
                                selected=selectedMPs(MP_object()),
                                inline=T,
                                choiceNames =labels,
                                choiceValues =seq_along(labels))

      shinyjs::delay(50, shinyjs::hide("FilterButton"))
      tagList(out)
    })

    output$defaults <- renderUI({
      i18n <- i18n()
      defaults <-  Default(MP_object())
      if (length(defaults)<1)
        shinyjs::delay(50, shinyjs::hide("reset_button"))

      tagList(
        shinyWidgets::actionBttn(ns("reset_button"),
                                 label=i18n$t("Reset Defaults"),
                                 icon("arrows-spin", verify_fa=FALSE),
                                 block=TRUE,
                                 style="fill",
                                 color='default',size='sm')
      )
    })

    output$filter_button <- renderUI({
      i18n <- i18n()
      shinyjs::hidden(shinyWidgets::actionBttn(ns("FilterButton"),
                                               label=i18n$t("FILTER"),
                                               icon("cogs", verify_fa=FALSE),
                                               block=TRUE,
                                               style="fill",
                                               color='danger',size='sm')
      )
    })


    # Reset OM Filters when new Slick loaded
    # and apply default  (if provided in Slick_Object)
    observeEvent(Slick_Object(), {
      if (!is.null(Slick_Object())) {
        Filter_Selected$MPs <- rep(TRUE, length(MP_object()@Label))
      }
      # Default MPs
      if (length(Default(MP_object()))>0) {
        default <- Default(MP_object())
        labels <- Label(MP_object(), i18n$get_translation_language())
        Filter_Selected$MPs <- seq_along(labels)[Default(MP_object())]
      }
    })

    # reset defaults
    observeEvent(input$reset_button, {
      updateCheckboxGroupInput(inputId=ns('Filter_MP'),
                               selected=selectedMPs(MP_object()))
    })

    # hide filter button after pressed
    observeEvent(input$FilterButton, {
      filterMPs(MP_object(), Filter_Selected, input)
      shinyjs::hide("FilterButton")
    })

    # show filter button if any changed
    show_filter <- observe({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        observeEvent(input$Filter_MP, {
          shinyjs::show("FilterButton")
        }, ignoreInit =TRUE)
      }
    })

    Filter_Selected

  })
}

## To be copied in the UI
# mod_Filter_MP_ui("Filter_MP_1")

## To be copied in the server
# mod_Filter_MP_server("Filter_MP_1")

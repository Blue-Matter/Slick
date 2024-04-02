selectedPMs <- function(pm_obj) {
  1:nrow(Metadata(pm_obj))
  # defaults <- Default(pm_obj)
  # if (length(defaults)<1) {
  #   out <- 1:length(Label(pm_obj))
  # } else {
  #   out <- defaults
  # }
  # out
}


filterPMs <- function(pm_obj, Filter_Selected, input) {
  metadata <- Metadata(pm_obj)

  keep <- rep(TRUE, nrow(metadata))
  keep <- 1:nrow(metadata)  %in% input$Filter_PM
  if (sum(keep)==0) {
    # select all if none are selected
    shinyjs::click('reset_button')
  } else {
    Filter_Selected$PMs <- keep
  }
}


#' Filter_PM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Filter_PM_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns('filters')),
    br(),
    uiOutput(ns('defaults'))
  )
}

#' Filter_PM Server Functions
#'
#' @noRd
mod_Filter_PM_server <- function(id, i18n, Slick_Object, slot){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Filter_Selected <- reactiveValues()

    PM_object <- reactive({
      fun <- get(slot)
      fun(Slick_Object())
    })

    output$filters <- renderUI({
      i18n <- i18n()
      metadata <- Metadata(PM_object(), i18n$get_translation_language())
      out <- checkboxGroupInput(ns('Filter_PM'),
                         label='',
                         selected=selectedPMs(PM_object()),
                         inline=T,
                         choiceNames = metadata$Code,
                         choiceValues =seq_along(metadata$Code))

      shinyjs::delay(50, shinyjs::hide("FilterButton"))
      tagList(out)
    })

    output$defaults <- renderUI({
      # i18n <- i18n()
      # defaults <-  Default(PM_object())
      # if (length(defaults)>0)
      #   shinyjs::delay(50, shinyjs::show("reset_button"))
      #
      shinyjs::hidden(
        shinyWidgets::actionBttn(ns("reset_button"),
                                 label=i18n$t("Reset Defaults"),
                                 icon("arrows-spin", verify_fa=FALSE),
                                 color='default',size='sm')
      )

    })

    # Reset OM Filters when new Slick loaded
    # and apply default  (if provided in Slick_Object)
    observeEvent(Slick_Object(), {
      if (!is.null(Slick_Object())) {
        Filter_Selected$PMs <- rep(TRUE, nrow(Metadata(PM_object())))
      }
      # Default PMs
      # if (length(Default(PM_object()))>0) {
      #   default <- Default(PM_object())
      #   labels <- Label(PM_object(), i18n$get_translation_language())
      #   Filter_Selected$PMs <- seq_along(labels)[Default(PM_object())]
      # }
    })

    # reset defaults
    observeEvent(input$reset_button, {
      updateCheckboxGroupInput(inputId='Filter_PM',
                               selected=selectedPMs(PM_object()))
    })

    observe({
      slick <- Slick_Object()
      if(length(input$Filter_PM)==0) {
        shinyjs::click('reset_button')
      }
      if (!is.null(slick)) {
        observeEvent(input$Filter_PM, {
          filterPMs(PM_object(), Filter_Selected, input)
        }, ignoreInit =TRUE)
      }
    })

    Filter_Selected
  })
}

## To be copied in the UI
# mod_Filter_PM_ui("Filter_PM_1")

## To be copied in the server
# mod_Filter_PM_server("Filter_PM_1")

selectedMPs <- function(mp_obj) {
  metadata <- Metadata(mp_obj)
  presets <- Preset(mp_obj)

  if (length(presets)<1) {
    return(1:nrow(metadata))
  }
  presets[[1]]
}

filterMPs <- function(mp_obj, Filter_Selected, input) {
  mp_selected <- input$Filter_MP
  metadata <- Metadata(mp_obj)

  keep <- rep(TRUE, nrow(metadata))
  keep <- (1:nrow(metadata)) %in% mp_selected

  if (sum(keep)==0) {
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
    uiOutput(ns('defaults'))
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
      metadata <- Metadata(MP_object(), i18n()$get_translation_language())

      out <- checkboxGroupInput(ns('Filter_MP'),
                                label='',
                                selected=selectedMPs(MP_object()),
                                inline=T,
                                choiceNames=metadata$Label,
                                choiceValues=seq_along(metadata$Label))
      tagList(out)
    })

    output$defaults <- renderUI({
      i18n <- i18n()
      presets <- Preset(MP_object())
      if (length(presets)>0)
        shinyjs::delay(50, shinyjs::show("reset_button"))

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
        Filter_Selected$MPs <- rep(TRUE, nrow(Metadata(MP_object())))
      }
      # Default MPs
      presets <- Preset(MP_object())
      if (length(presets)>0) {
        Filter_Selected$MPs <-  presets[[1]]
      }
    })

    # reset defaults
    observeEvent(input$reset_button, {
      updateCheckboxGroupInput(inputId='Filter_MP',
                               selected=selectedMPs(MP_object()))
    })

    observe({
      slick <- Slick_Object()
      if(length(input$Filter_MP)==0) {
        shinyjs::click('reset_button')
      }
      if (!is.null(slick)) {
        observeEvent(input$Filter_MP, {
          filterMPs(MP_object(), Filter_Selected, input)
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

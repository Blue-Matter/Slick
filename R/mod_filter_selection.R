#' mp_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('selections')),
    uiOutput(ns('presets')),
    uiOutput(ns('globalMPsettings')),
    br()
  )
}

#' mp_selection Server Functions
#'
#' @noRd
mod_filter_selection_server <- function(id, i18n, slick, slot, minN, include=TRUE,
                                        incIcons=TRUE, icon='circle',
                                        home_session=NULL, includeGlobalMPSettings=TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected <- reactiveValues()

    object <- reactive({
      req(slick())
      fun <- get(slot)
      object <- fun(slick())
      object
    })


    metadata <- reactive({
      Metadata(object(), i18n()$get_translation_language())
    })

    presets <- reactive({
      Preset(object())
    })

    # preset buttons (if they exist) and an invisible reset button
    # resets if none are selected
    output$presets <- renderUI({
      req(slick())
      if (!include)   return(NULL)
      i18n <- i18n()
      presets <- presets()
      ll <- NULL

      if (length(presets)>0) {
        btn_names <- names(presets)
        ll <- lapply(1:length(presets), function(i) {
          shinyWidgets::actionBttn(ns(paste0("preset",i)),
                                   label=btn_names[i],
                                   icon=icon('fa-arrows-rotate', class='fa-solid'),
                                   color='default',size='sm')
        })
      }
      tagList(
        shinyjs::hidden(
          shinyWidgets::actionBttn(ns("reset_button"),
                                   label=i18n$t("Reset Defaults"),
                                   icon("arrows-spin", verify_fa=FALSE),
                                   color='default',size='sm')
        ),
        tagList(ll)
      )
    })

    # observe if preset buttons are pressed
    # maximum of 4 preset buttons
    observeEvent(input[['preset1']],{
      selected <- presets()[[1]]
      updateCheckboxGroupInput(inputId="filter1",
                               selected=selected)

    }, ignoreInit =TRUE)

    observeEvent(input[['preset2']],{
      selected <- presets()[[2]]
      updateCheckboxGroupInput(inputId="filter1",
                               selected=selected)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset3']],{
      selected <- presets()[[3]]
      updateCheckboxGroupInput(inputId="filter1",
                               selected=selected)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset4']],{
      selected <- presets()[[4]]
      updateCheckboxGroupInput(inputId="filter1",
                               selected=selected)
    }, ignoreInit =TRUE)

    observeEvent(input$reset_button, {
      selected <- initial_selected()
      updateCheckboxGroupInput(inputId="filter1",
                               selected=selected)
    })

    choice_names <- reactive({
      req(slick())
      i18n <- i18n()
      ll <- list()
      if (slot=='MPs') {
        mp_metadata <- metadata()
        if (length(mp_metadata)>0) {
          for (i in 1:nrow(mp_metadata)) {
            if (incIcons) {
              if (icon=='circle') {
                ll[[i]] <- HTML(paste(icon('fa-circle', class='fa-solid',
                                           style=paste('color:', mp_metadata$Color[i], ';')),  mp_metadata$Code[i]))
              } else if (icon=='hexagon') {
                ll[[i]] <- HTML(paste(icon('fa-hexagon', class='fa-solid',
                                           style=paste('color:', mp_metadata$Color[i], ';')),  mp_metadata$Code[i]))
              } else {
                ll[[i]] <- HTML(paste(icon('fa-chart-line', class='fa-solid',
                                           style=paste('color:', mp_metadata$Color[i], ';')),  mp_metadata$Code[i]))
              }

            } else {
              ll[[i]] <- mp_metadata$Code[i]
            }

          }
        }
      } else {
        ll <- metadata()$Code
      }
      ll
    })

    initial_selected <- reactive({
      presets <- Preset((object()))
      if (length(presets)<1) {
        return(1:nrow(metadata()))
      }
      presets[[1]]
    })

    label <- reactive({
      i18n <- i18n()
      if (slot=='MPs') {
        return(h4(i18n$t('Management Procedures')))
      }
      h4(i18n$t('Performance Indicators'))
    })

    output$selections <- renderUI({
      if (!include) return(NULL)

      i18n <- i18n()
      choiceNames <- choice_names()
      if (length(choiceNames)>0) {
        tagList(
          shiny::checkboxGroupInput(
            ns('filter1'),
            inline=TRUE,
            label=actionLink(ns('label'), label=label()),
            selected=initial_selected(),
            choiceNames=choice_names(),
            choiceValues=seq_along(choice_names())
          )
        )
      }
    })

    output$globalMPsettings <- renderUI({
      if (slot !='MPs') return(NULL)
      if (!includeGlobalMPSettings) return(NULL)
      i18n <- i18n()
      actionLink(ns('openGlobal'), h4(i18n$t('Global MP Settings')))
    })

    observeEvent(input$openGlobal, {
      shinydashboardPlus::updateControlbar('controlbar', home_session)
    })

    outputOptions(output, "selections", suspendWhenHidden = FALSE)

    observeEvent(input$label, {
      if (slot=='MPs') {
        shinyjs::click('mpdropdown', asis=TRUE)
      } else {
        shinyjs::click('pmdropdown', asis=TRUE)
        shinyjs::delay(10,
          updateTabsetPanel(session=home_session,
                            inputId="pmtabsetpanel", selected = slot)
        )

      }
    })

    observe({
      Filter_Selected$selected <- input$filter1
      if (length(Filter_Selected$selected)<minN) {
        shinyjs::click('reset_button')
      }
    })

    observeEvent(slick(), {
      shinyjs::click('reset_button')
    })



    reactive(
      Filter_Selected$selected
    )
  })
}




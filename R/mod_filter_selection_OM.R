filterOMs <- function(slick, Filter_Selected, input) {
  keep <- array(TRUE,dim(Design(slick)))
  for(fac in 1:ncol(Design(slick))) {
    design <- Design(OMs(slick)) |> as.data.frame()
    design[sapply(design, is.character)] <- lapply(design[sapply(design, is.character)],
                                                   function(x) {

      x <- factor(x, ordered=TRUE, levels=unique(x))

    })
    design[] <- lapply(design,  as.numeric)
    factor_numbers <- design[,fac]
    factor_numbers <- as.numeric(factor(factor_numbers))
    selected_factors <- as.numeric(input[[paste0("filter",fac)]])
    keep[,fac] <- factor_numbers%in% selected_factors

  }

  if (any(colSums(keep)==0)) {
    # select all if none are selected
    shinyjs::click('reset_button')
  } else {
    Filter_Selected$selected <- which(apply(keep,1,all))
  }
}

updateCheckbox_OM <- function(object, preset=1) {
  factors <- colnames(Design(object))
  for(i in 1:length(factors)) {
    selected <- initial_selected_OM(object, preset, factor=i)
    updateCheckboxGroupInput(inputId=paste0("filter",i),
                             selected=selected)
  }
}

initial_selected_OM <- function(object, preset=1, factor=1, include_preset=TRUE) {
  metadata <- Factors(object)
  factors <- unique(metadata$Factor)
  metadata <- metadata |> dplyr::filter(Factor==factors[factor])
  presets <- Preset(object)
  if (!include_preset) {
    return(1:nrow(metadata))
  }
  if (length(presets)<1) {
    return(1:nrow(metadata))
  }
  if (length(presets[[preset]])<factor) {
    return(NULL)
  }
  presets[[preset]][[factor]]
}



#' om_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_selection_om_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('selections')),
    uiOutput(ns('presets')),
    br()
  )
}

#' om_selection Server Functions
#'
#' @noRd
mod_filter_selection_om_server <- function(id, i18n, slick, include_preset=TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    object <- reactive({
      slick <- slick()
      if (!is.null(slick))
        OMs(slick())
    })

    presets <- reactive({
      Preset(object())
    })

    # observe if preset buttons are pressed
    # maximum of 4 preset buttons
    observeEvent(input[['preset1']],{
      updateCheckbox_OM(object(), 1)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset2']],{
      updateCheckbox_OM(object(), 2)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset3']],{
      updateCheckbox_OM(object(), 3)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset4']],{
      updateCheckbox_OM(object(), 4)
    }, ignoreInit =TRUE)


    observeEvent(input[['preset5']],{
      updateCheckbox_OM(object(), 5)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset6']],{
      updateCheckbox_OM(object(), 6)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset7']],{
      updateCheckbox_OM(object(), 7)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset8']],{
      updateCheckbox_OM(object(), 8)
    }, ignoreInit =TRUE)


    # preset buttons (if they exist) and an invisible reset button
    # resets if none are selected
    output$presets <- renderUI({
      if (!include_preset)
        return(NULL)
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

    factors <- reactive({
      req(object())
      Factors(object())
    })
    om_checkboxes <- reactive({
      req(object())
      factors <- unique(factors()$Factor)
      if (length(factors)>0) {
        ll <- lapply(1:length(factors), function(i) {
          levels <- factors() |> dplyr::filter(Factor==factors[i]) |>
            dplyr::select(Level)
          checkboxGroupInput(ns(paste0("filter",i)),
                             label=factors[i],
                             selected=initial_selected_OM(object(), preset=1, factor=i, include_preset),
                             inline=TRUE,
                             choiceNames=levels$Level,
                             choiceValues=seq_along(levels$Level))
        })
        ll
      }
    })

    observeEvent(input$reset_button, {
      updateCheckbox_OM(object(), preset=1)
    })

    output$selections <- renderUI({
      i18n <- i18n()
      tagList(
        actionLink(ns('label'), h4(i18n$t('Operating Models'))),
        om_checkboxes()
      )

    })

    outputOptions(output, "selections", suspendWhenHidden = FALSE)

    observeEvent(input$label, {
      shinyjs::click('omdropdown', asis=TRUE)
    })

    Filter_Selected <- reactiveValues()

    observe({
      slick <- slick()
      if (!is.null(slick)) {
        if (inherits(object(), 'OMs')) {
          FilterNames <- paste0("filter",1:ncol(Design(slick)))
          observeEvent(sapply(FilterNames, function(x) input[[x]]),{
            filterOMs(slick, Filter_Selected, input)
          }, ignoreInit =TRUE)
        } else {
          metadata <- Design(object())
          keep <- rep(TRUE, nrow(metadata))
          keep <- 1:nrow(metadata) %in% input$filter1
          if (sum(keep)==0) {
            # select all if none are selected
            shinyjs::click('reset_button')
          } else {
            Filter_Selected$selected <- which(keep)
          }
        }
      }
    })

    reactive(Filter_Selected$selected)
  })
}

## To be copied in the UI
# mod_om_selection_ui("om_selection_1")

## To be copied in the server
# mod_om_selection_server("om_selection_1")

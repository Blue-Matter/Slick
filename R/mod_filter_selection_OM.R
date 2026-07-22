filterOMs <- function(slick, Filter_Selected, input, applying_preset=NULL) {
  if (!is.null(applying_preset) && isTRUE(applying_preset()))
    return(invisible(NULL))

  oms <- OMs(slick)
  codes <- om_level_codes(oms)

  keep <- array(TRUE, dim(codes))
  for(fac in seq_len(ncol(codes))) {
    selected_factors <- as.numeric(input[[paste0("filter",fac)]])
    keep[,fac] <- codes[,fac] %in% selected_factors
  }

  if (any(colSums(keep)==0)) {
    # select all if none are selected
    shinyjs::click('reset_button')
  } else {
    Filter_Selected$selected <- which(apply(keep,1,all))
  }
}

updateCheckbox_OM <- function(object, preset=1, Filter_Selected=NULL, applying_preset=NULL) {
  presets <- Preset(object)
  om_rows <- if (length(presets)>=preset) presets[[preset]] else 1:nrow(Design(object))

  if (!is.null(applying_preset)) applying_preset(TRUE)
  if (!is.null(Filter_Selected)) Filter_Selected$selected <- om_rows

  factors <- colnames(Design(object))
  for(i in seq_along(factors)) {
    selected <- initial_selected_OM(object, preset, factor=i)
    updateCheckboxGroupInput(inputId=paste0("filter",i),
                             selected=selected)
  }

  # give the client time to settle before re-enabling the manual
  # AND-recompute observer (see filterOMs())
  if (!is.null(applying_preset)) shinyjs::delay(150, applying_preset(FALSE))
}

initial_selected_OM <- function(object, preset=1, factor=1, include_preset=TRUE) {
  metadata <- Factors(object)
  factors <- unique(metadata$Factor)
  factor_levels <- metadata |> dplyr::filter(Factor==factors[factor])
  if (!include_preset) {
    return(1:nrow(factor_levels))
  }
  presets <- Preset(object)
  if (length(presets)<1 || length(presets)<preset) {
    return(1:nrow(factor_levels))
  }
  om_rows <- presets[[preset]]
  if (length(om_rows)<1) {
    return(1:nrow(factor_levels))
  }
  codes <- om_level_codes(object)
  sort(unique(codes[om_rows, factor]))
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

    Filter_Selected <- reactiveValues()
    # TRUE while a preset's (or the initial default's) checkbox echo is
    # still settling - see filterOMs()/updateCheckbox_OM()
    applying_preset <- reactiveVal(TRUE)

    observeEvent(object(), {
      req(object())
      om_rows <- if (length(Preset(object()))>=1) Preset(object())[[1]] else 1:nrow(Design(object()))
      Filter_Selected$selected <- om_rows
      shinyjs::delay(150, applying_preset(FALSE))
    })

    # observe if preset buttons are pressed
    # maximum of 4 preset buttons
    observeEvent(input[['preset1']],{
      updateCheckbox_OM(object(), 1, Filter_Selected, applying_preset)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset2']],{
      updateCheckbox_OM(object(), 2, Filter_Selected, applying_preset)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset3']],{
      updateCheckbox_OM(object(), 3, Filter_Selected, applying_preset)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset4']],{
      updateCheckbox_OM(object(), 4, Filter_Selected, applying_preset)
    }, ignoreInit =TRUE)


    observeEvent(input[['preset5']],{
      updateCheckbox_OM(object(), 5, Filter_Selected, applying_preset)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset6']],{
      updateCheckbox_OM(object(), 6, Filter_Selected, applying_preset)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset7']],{
      updateCheckbox_OM(object(), 7, Filter_Selected, applying_preset)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset8']],{
      updateCheckbox_OM(object(), 8, Filter_Selected, applying_preset)
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
      updateCheckbox_OM(object(), preset=1, Filter_Selected, applying_preset)
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

    observe({
      slick <- slick()
      if (!is.null(slick)) {
        if (inherits(object(), 'OMs')) {
          FilterNames <- paste0("filter",1:ncol(Design(slick)))
          observeEvent(sapply(FilterNames, function(x) input[[x]]),{
            filterOMs(slick, Filter_Selected, input, applying_preset)
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


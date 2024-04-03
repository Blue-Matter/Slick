selectedOMs <- function(i, oms, preset=1) {
  metadata <- Metadata(oms)
  factors <- unique(metadata$Factor)
  metadata <- metadata |> dplyr::filter(Factor==factors[i])
  presets <- Preset(oms)
  if (length(presets)<1) {
    return(seq_along(metadata))
  }
  if (length(presets[[preset]])<i) {
    return(NULL)
  }
  presets[[preset]][[i]]
}

filterOMs <- function(slick, Filter_Selected, input) {
  keep <- array(T,dim(Design(slick)))
  for(fac in 1:ncol(Design(slick))) {
    design <- Design(OMs(slick))
    design[sapply(design, is.character)] <- lapply(design[sapply(design, is.character)],  function(x) {
      x <- factor(x, ordered=TRUE, levels=unique(x))
    })
    design[] <- lapply(design,  as.numeric)
    factor_numbers <- design[,fac]
    selected_factors <- as.numeric(input[[paste0("Filter_OM",fac)]])
    keep[,fac] <- factor_numbers%in% selected_factors
  }

  if (any(colSums(keep)==0)) {
    # select all if none are selected
    shinyjs::click('reset_button')

  } else {
    Filter_Selected$OMs <- apply(keep,1,all)
  }

}


#' Filter_OM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Filter_OM_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns('filters')),
    br(),
    uiOutput(ns('presets'))
  )
}

#' Filter_OM Server Functions
#'
#' @noRd
mod_Filter_OM_server <- function(id, i18n, Slick_Object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Filter_Selected <- reactiveValues()

    OM_object <- reactive({
      OMs(Slick_Object())
    })

    output$filters <- renderUI({
      metadata <- Metadata(OM_object(), i18n()$get_translation_language())
      factors <- unique(metadata$Factor)
      ll <- lapply(1:length(factors), function(i) {
        levels <- metadata |> dplyr::filter(Factor==factors[i]) |>
          dplyr::select(Level)
        checkboxGroupInput(ns(paste0("Filter_OM",i)),
                           label=factors[i],
                           selected=selectedOMs(i, OM_object()),
                           inline=TRUE,
                           choiceNames=levels$Level,
                           choiceValues=seq_along(levels$Level))
      })
      tagList(ll)
    })

    output$presets <- renderUI({
      i18n <- i18n()
      presets <- Preset(OM_object())

      if (length(presets)>0) {
        btn_names <- names(presets)
        ll <- lapply(1:length(presets), function(i) {
          shinyWidgets::actionBttn(ns(paste0("preset",i)),
                                   label=btn_names[i],
                                   icon("arrows-spin", verify_fa=FALSE),
                                   color='default',size='sm')
        })
      }
        # shinyjs::delay(50, shinyjs::show("reset_button"))

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


    observe({
      slick <- Slick_Object()
      presets <- Preset(OM_object())
      if (!is.null(slick)) {
        if (length(presets)>0) {
          print('here')
          btn_names <- names(presets)
          preset_names <- paste0("preset",1:length(names(presets)))

          for(i in 1:length(btn_names)) {
            updateCheckboxGroupInput(inputId=paste0("preset",i),
                                     selected=selectedOMs(i, OM_object(), i))
          }


        }
      }
    })

    # Reset OM Filters when new Slick loaded
    # and apply default OMs (if provided in Slick_Object)
    observeEvent(Slick_Object(), {
      if (!is.null(Slick_Object())) {
        Filter_Selected$OMs <- rep(TRUE, nrow(Design(Slick_Object())))
      }
      # Default OMs
      presets <- Preset(OM_object())
      if (length(presets)>0) {
        slick <- Slick_Object()
        default <- presets[[1]]
        design <- Design(OM_object())
        keep <- array(T,dim(design))
        for(fac in 1:ncol(design)) {
          design[sapply(design, is.character)] <- lapply(design[sapply(design, is.character)],  function(x) {
            x <- factor(x, ordered=TRUE, levels=unique(x))
          })
          design[] <- lapply(design,  as.numeric)
          factor_numbers <- design[,fac]
          selected_factors <- default[[fac]]
          keep[,fac] <- factor_numbers%in% selected_factors
        }
        Filter_Selected$OMs <- apply(keep,1,all)
      }
    })

    # reset defaults
    observeEvent(input$reset_button, {
      slick <- Slick_Object()
      oms <- OMs(slick)
      factors <- colnames(oms@Design)
      for(i in 1:length(factors)) {
        updateCheckboxGroupInput(inputId=paste0("Filter_OM",i),
                                 selected=selectedOMs(i, oms))
      }
    })

    observe({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        FilterNames <- paste0("Filter_OM",1:ncol(Design(slick)))
        observeEvent(sapply(FilterNames, function(x) input[[x]]),{
          filterOMs(slick, Filter_Selected, input)
        }, ignoreInit =TRUE)
      }
    })

    Filter_Selected
  })
}




## To be copied in the UI
# mod_Filter_OM_ui("Filter_OM_1")

## To be copied in the server
# mod_Filter_OM_server("Filter_OM_1", i18n, Slick_Object)


make_checkboxes <- function(object, ns) {
  metadata <- Metadata(object)
  if (!is.null(metadata$Code)) {
    selected <- initial_selected(object)
    ll <- list(
      checkboxGroupInput(ns('filter1'),
                         label='',
                         selected=selected,
                         inline=T,
                         choiceNames=metadata$Code,
                         choiceValues=seq_along(metadata$Code))
    )
  }
  if (!is.null(metadata$Factor)) {
    factors <- unique(metadata$Factor)
    ll <- lapply(1:length(factors), function(i) {
      levels <- metadata |> dplyr::filter(Factor==factors[i]) |>
        dplyr::select(Level)
      selected <- initial_selected(object, factor=i)
      checkboxGroupInput(ns(paste0("filter",i)),
                         label=factors[i],
                         selected=selected,
                         inline=TRUE,
                         choiceNames=levels$Level,
                         choiceValues=seq_along(levels$Level))
    })
  }
  ll
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

initial_selected_OM <- function(object, preset=1, factor=1) {
  metadata <- Metadata(object)
  factors <- unique(metadata$Factor)
  metadata <- metadata |> dplyr::filter(Factor==factors[factor])
  presets <- Preset(object)
  if (length(presets)<1) {
    return(1:nrow(metadata))
  }
  if (length(presets[[preset]])<factor) {
    return(NULL)
  }
  presets[[preset]][[factor]]
}

# initial_selected_MP <- function(object, preset=1) {
#   metadata <- Metadata(object)
#   presets <- Preset((object))
#   if (length(presets)<1) {
#     return(1:nrow(metadata))
#   }
#   presets[[preset]]
# }

initial_selected <- function(object, preset=1, factor=1) {
  if (inherits(object, 'OMs')) {
    return(initial_selected_OM(object, preset, factor))
  }
  # if (inherits(object, 'MPs')) {
  #   return(initial_selected_MP(object, preset))
  # }
  # 1:nrow(Metadata(object))
  metadata <- Metadata(object)
  presets <- Preset((object))
  if (length(presets)<1) {
    return(1:nrow(metadata))
  }
  presets[[preset]]
}





updateCheckbox_OM <- function(object, preset=1) {
  factors <- colnames(Design(object))
  for(i in 1:length(factors)) {
    selected <- initial_selected_OM(object, preset, factor=i)
    updateCheckboxGroupInput(inputId=paste0("filter",i),
                             selected=selected)
  }
}

updateCheckbox <- function(object, preset=1) {
  if (inherits(object, 'OMs')) {
    updateCheckbox_OM(object)
  }
  selected <- initial_selected(object, preset)
  updateCheckboxGroupInput(inputId="filter1",
                           selected=selected)
}

#' Filter_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Filter_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns('filters')),
    br(),
    uiOutput(ns('presets'))
  )
}



#' Filter_select Server Functions
#'
#' @noRd
mod_Filter_select_server <- function(id, i18n, Slick_Object, slot, include=TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected <- reactiveValues()

    object <- reactive({
      fun <- get(slot)
      fun(Slick_Object())
    })

    metadata <- reactive({
      Metadata(object(), i18n()$get_translation_language())
    })

    checkboxes <- reactive({
      slick <- Slick_Object()
      make_checkboxes(object(), ns)
    })

    observeEvent(Slick_Object(), checkboxes())

    output$filters <- renderUI({
      if (!include) return(NULL)
      ll <- checkboxes() # make_checkboxes(object(), ns)
      tagList(ll)
    })


    # preset buttons (if they exist) and an invisible reset button
    # resets if none are selected
    output$presets <- renderUI({
      if (!include) return(NULL)
      i18n <- i18n()
      presets <- Preset(object())
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
      updateCheckbox(object(), 1)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset2']],{
      updateCheckbox(object(), 2)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset3']],{
      updateCheckbox(object(), 3)
    }, ignoreInit =TRUE)

    observeEvent(input[['preset4']],{
      updateCheckbox(object(), 4)
    }, ignoreInit =TRUE)

    # Reset Filters when new Slick loaded
    # and apply default  (if provided in Slick_Object)
    observeEvent(Slick_Object(), {
      if (!is.null(Slick_Object())) {
        if (inherits(object(), 'OMs')) {
          Filter_Selected$OMs <- rep(TRUE, nrow(Design(object())))
        } else {
          Filter_Selected$selected <- rep(TRUE, nrow(Metadata(object())))
        }
        Filter_Selected$initial <- TRUE
      }
      # Initial selection - all or Preset 1
      presets <- Preset(object())
      if (length(presets)>0) {
        if (inherits(object(), 'OMs')) {
          design <- Design(object())
          factors <- colnames(design)
          keep <- array(T,dim(design))
          for(i in seq_along(factors)) {
            design[sapply(design, is.character)] <- lapply(design[sapply(design, is.character)],  function(x) {
              x <- factor(x, ordered=TRUE, levels=unique(x))
            })
            design[] <- lapply(design,  as.numeric)
            factor_numbers <- design[,i]
            selected <- initial_selected(object(), 1, i)
            keep[,i] <- factor_numbers%in% selected
          }
          Filter_Selected$selected <- apply(keep,1,all)
        } else {
          selected <- initial_selected(object(), preset=1, factor=1)
          Filter_Selected$selected <- selected
        }
      }
    })

    # reset defaults
    observeEvent(input$reset_button, {
      updateCheckbox(object())
    })

    observe({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        if (inherits(object(), 'OMs')) {
          FilterNames <- paste0("filter",1:ncol(Design(slick)))
          observeEvent(sapply(FilterNames, function(x) input[[x]]),{
            filterOMs(slick, Filter_Selected, input)
          }, ignoreInit =TRUE)
        } else {
          metadata <- Metadata(object())
          keep <- rep(TRUE, nrow(metadata))
          keep <- 1:nrow(metadata) %in% input$filter1
          if (sum(keep)==0) {
            # select all if none are selected
            shinyjs::click('reset_button')
          } else {
            Filter_Selected$selected <- keep
          }
        }

      }
    })


    Filter_Selected
  })
}

## To be copied in the UI
# mod_Filter_select_ui("Filter_select_1")

## To be copied in the server
# mod_Filter_select_server("Filter_select_1")

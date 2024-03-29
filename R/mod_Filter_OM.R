selectedOMs <- function(i, OM) {
  defaults <- Default(OM)
  if (length(defaults)<1) {
    out <- 1:length(Label(OM)[[i]])
  } else {
    if (length(defaults)<i) {
      out <- NULL
    } else {
      out <- defaults[[i]]
    }
  }
  out
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
    fluidRow(
      column(3, uiOutput(ns('defaults'))),
      column(3, uiOutput(ns('filter_button')))
    )
  )
}

#' Filter_OM Server Functions
#'
#' @noRd
mod_Filter_OM_server <- function(id, i18n, Slick_Object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Filter_Selected <- reactiveValues()

    output$filters <- renderUI({
      i18n <- i18n()
      oms <- OMs(Slick_Object())
      labels <- Label(oms, i18n$get_translation_language())
      factors <- colnames(oms@Design)
      ll <- lapply(1:length(factors), function(i) {
        checkboxGroupInput(ns(paste0("Filter_OM",i)),
                           label=factors[i],
                           selected=selectedOMs(i, oms),
                           inline=TRUE,
                           choiceNames=labels[[i]],
                           choiceValues=1:length(labels[[i]])
        )
      })
      shinyjs::delay(50, shinyjs::hide("FilterButton"))
      tagList(ll)
    })

    output$defaults <- renderUI({
      i18n <- i18n()
      defaults <- Default(OMs(Slick_Object()))
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
    # and apply default OMs (if provided in Slick_Object)
    observeEvent(Slick_Object(), {
      if (!is.null(Slick_Object())) {
        Filter_Selected$OMs <- rep(TRUE, nrow(Design(Slick_Object())))
      }
      # Default OMs
      if (length(Default(OMs(Slick_Object())))>0) {
        slick <- Slick_Object()
        default <- Default(OMs(slick))
        design <- Design(OMs(slick))
        keep <- array(T,dim(Design(slick)))
        for(fac in 1:ncol(Design(slick))) {
          design <- Design(OMs(slick))
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
      golem::print_dev('reset clicked')
      slick <- Slick_Object()
      oms <- OMs(slick)
      factors <- colnames(oms@Design)
      for(i in 1:length(factors)) {
        updateCheckboxGroupInput(inputId=paste0("Filter_OM",i),
                                 selected=selectedOMs(i, oms))
      }
    })

    # hide filter button after pressed
    observeEvent(input$FilterButton, {
      filterOMs(Slick_Object(), Filter_Selected, input)
      shinyjs::hide("FilterButton")
      shinydashboardPlus::updateBoxSidebar(id='filtersidebar', session = shiny::getDefaultReactiveDomain())
    })

    # show filter button if any changed
    show_filter <- observe({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        FilterNames <- paste0("Filter_OM",1:ncol(Design(slick)))
        observeEvent(sapply(FilterNames, function(x) input[[x]]),{
          shinyjs::show("FilterButton")
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

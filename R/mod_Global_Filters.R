filter_mps <- function(slick, mps) {

  mps <- as.numeric(mps)
  if (is.null(slick))
    return(slick)
  if (length(mps)<1)
    return(slick)

  metadata <- slick |> MPs() |> Metadata()
  Metadata(MPs(slick)) <- metadata[mps,]

  val <- slick |> Boxplot() |> Value()
  if (!all(is.na(val))) {
    Value(Boxplot(slick)) <- val[,,mps,,drop=FALSE]
  }


  val <- slick |> Kobe() |> Value()
  if (!all(is.na(val))) {
    Value(Kobe(slick)) <- val[,,mps,,,drop=FALSE]
  }

  val <- slick |> Quilt() |> Value()
  if (!all(is.na(val))) {
    Value(Quilt(slick)) <- val[,mps,,drop=FALSE]
  }

  val <- slick |> Spider() |> Value()
  if (!all(is.na(val))) {
    Value(Spider(slick)) <- val[,mps,,drop=FALSE]
  }

  val <- slick |> Timeseries() |> Value()
  if (!all(is.na(val))) {
    Value(Timeseries(slick)) <- val[,,mps,,,drop=FALSE]
  }

  val <- slick |> Tradeoff() |> Value()
  if (!all(is.na(val))) {
    Value(Tradeoff(slick)) <- val[,mps,,drop=FALSE]
  }

  slick
}

#' Global_Filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Global_Filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('page')),
    br(),
    uiOutput(ns('MPcolors'))
  )
}

#' Global_Filters Server Functions
#'
#' @noRd
mod_Global_Filters_server <- function(id, i18n, Slick_Object, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    slick_in <- reactiveVal()

    observeEvent(Slick_Object(), {
      slick_in(Slick_Object())
    })


    mp_colors <- mod_MP_Color_server("MP_Color_1",i18n, Slick_Object)
    selected_mps <- mod_filter_selection_server("filter_mp", i18n, slick_in,
                                                slot='MPs', minN=1,
                                                includeGlobalMPSettings=FALSE)

    output$heading <- renderUI({
      i18n <- i18n()
      tagList(
        h3(i18n$t('Global MP Settings')),
        p(i18n$t('These Management Procedure settings will apply to all plots.')),
        p(i18n$t('MPs that are deselected here will not be displayed in any plots.')),
        p(i18n$t('The MP Color Settings will change the colors of the MPs in all plots.'))
      )
    })

    output$filters <- renderUI({
      i18n <- i18n()
      tagList(
        div(class='multicol',
               mod_filter_selection_ui(ns('filter_mp'))
        )
      )
    })

    output$filter_button <- renderUI({
      i18n <- i18n()
      shinyjs::hidden(
        shinyWidgets::actionBttn(ns("FilterButton"),
                                 label=i18n$t("Apply Global MP Filter"),
                                 icon("filter", verify_fa=FALSE),
                                 color='danger',size='sm',
                                 block=TRUE, style="fill")
      )
    })

    output$page <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      if (is.null(slick)) {
        return(tagList(
          column(12,
                 tagList(
                   uiOutput(ns('heading')),
                   br(),
                   h4(i18n$t('Please load a Slick object'))
                 )
          )
        ))
      }

      tagList(
        column(12,
               uiOutput(ns('heading')),
               uiOutput(ns('filters')),
               uiOutput(ns('filter_button'))
        )
      )
    })

    output$MPcolors <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      if (!is.null(slick)) {
        tagList(
          mod_MP_Color_ui(ns("MP_Color_1"))
        )
      }

    })

    Filter_Selected <- reactiveValues(MPs=1)

    observeEvent(mp_colors(), {
      slick <- isolate(Slick_Object())
      if (!is.null(slick)) {
        mps <- MPs(slick)
        metadata <- Metadata(mps)
        metadata$Color <- isolate(mp_colors())
        Metadata(MPs(slick)) <- metadata
        Filter_Selected$MPs <- 1:nrow(metadata)
        slick_in(slick)
      }
    })

    allMPs <- reactive({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        mps <- MPs(slick)
        1:nrow(Metadata(mps))
      }
    })


    observeEvent(allMPs(), {
      Filter_Selected$MPs <- allMPs()
    }, ignoreInit = TRUE)


    observeEvent(selected_mps(), {
      shinyjs::show("FilterButton")
    }, ignoreInit = TRUE)


    # filter slick object
    observeEvent(input$FilterButton, {
      shinyjs::hide("FilterButton")
      Filter_Selected$MPs <- as.numeric(selected_mps())
      shinydashboardPlus::updateControlbar('controlbar', session=parent_session)
    })

    globalslick <- reactive({
      filter_mps(slick_in(), Filter_Selected$MPs)
    })


    slick_out <- reactiveVal(Slick_Object)

    observeEvent(globalslick(), {
      slick_out(globalslick())
    })
    slick_out



  })
}

## To be copied in the UI
# mod_Global_Filters_ui("Global_Filters_1")

## To be copied in the server
# mod_Global_Filters_server("Global_Filters_1")

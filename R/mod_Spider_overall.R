#' Spider_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('overall_Spider'))
  )
}

#' Spider_overall Server Functions
#'
#' @noRd
mod_Spider_overall_server <- function(id, i18n, filtered_slick,
                                      nOM, nMP, nPM, parent_session,
                                      relative_scale=relative_scale,
                                      window_dims){

  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$overall_Spider <- renderUI({
      i18n <- i18n()
      tagList(
        shinycssloaders::withSpinner(
          plotOutput(ns('spider_plot'), height=plot_height())
        )
      )
    })

    plot_height_calc <- reactive({
      dims <- window_dims()
      dims[1]*0.4
    })

    plot_height <- plot_height_calc |> debounce(500)

    output$spider_plot <- renderPlot({
      if (!is.null(relative_scale()))
        Spiderplot_all_MPs(filtered_slick(), relative_scale=relative_scale())
    }, width=function() {
      dims <- window_dims()
      plot_height()
    }, height=function() {
      plot_height()
    })

  })
}

## To be copied in the UI
# mod_Spider_overall_ui("Spider_overall_1")

## To be copied in the server
# mod_Spider_overall_server("Spider_overall_1")

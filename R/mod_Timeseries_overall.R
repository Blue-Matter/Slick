#' Timeseries_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Timeseries_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot'))
  )
}

#' Timeseries_overall Server Functions
#'
#' @noRd
mod_Timeseries_overall_server <- function(id, i18n, filtered_slick,
                                          pm_ind, yrange,
                                          window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    timeseriesplot <- reactive({
      req(pm_ind())
      plotTimeseries(filtered_slick(), pm_ind())
    })

    output$plot1 <- renderPlot({
       p <- timeseriesplot()
       p + ggplot2::coord_cartesian(ylim=yrange())

    })

    output$plot <- renderUI({
      tagList(
        br(),
        shinycssloaders::withSpinner(
          plotOutput(ns('plot1'), width=plot_width(), height=plot_height())
        )
      )

    })

    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.6
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    plot_height_calc <- reactive({
      dd <- window_dims()
      val <- dd[2] * 0.6
      paste0(val, 'px')
    })

    plot_height <- plot_height_calc |> debounce(500)



  })
}





## To be copied in the UI
# mod_Timeseries_overall_ui("Timeseries_overall_1")

## To be copied in the server
# mod_Timeseries_overall_server("Timeseries_overall_1")

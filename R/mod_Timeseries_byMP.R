#' Timeseries_byMP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Timeseries_byMP_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class='top_border',
        uiOutput(ns('page'))
    )
  )
}

#' Timeseries_byMP Server Functions
#'
#' @noRd
mod_Timeseries_byMP_server <- function(id, i18n, filtered_slick,
                                       pm_ind, yrange, nMP,
                                       window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$page <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nMP(), function(mm) {
          plotname <- paste("plot", mm, sep="")
          shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width='400px',
                                                  height='300px'))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })


    plot_width_text <- reactive({
      paste0('width: ', '400px;', '; height: 300px;')
    })

    make_plots <- reactive({
      if (is.null(filtered_slick()))
        return(NULL)
      if (is.null(pm_ind()))
        return(NULL)
      if (is.null(yrange()))
        return(NULL)
      dd <- filtered_slick() |> Timeseries() |> Value() |>  dim()
      plot_list <- list()
      if (dd[3]==nMP()) {
        for (i in 1:nMP()) {
          plot_list[[i]] <- Timeseries_plot(filtered_slick(), pm_ind(), yrange(), i)
        }
      }
      plot_list
    })


    observeEvent(make_plots(), {
      thisplot <- make_plots()
      if (length(thisplot)>0) {
        for (i in 1:nMP()) {
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")
            output[[plotname]] <- renderPlot({
              thisplot[[my_i]]
            })
          })
        }
      }
    })



  })
}

## To be copied in the UI
# mod_Timeseries_byMP_ui("Timeseries_byMP_1")

## To be copied in the server
# mod_Timeseries_byMP_server("Timeseries_byMP_1")


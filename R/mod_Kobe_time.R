#' Kobe_time UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Kobe_time_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('page'))
  )
}

#' Kobe_time Server Functions
#'
#' @noRd
mod_Kobe_time_server <- function(id,i18n, filtered_slick,
                                 plottype,
                                 nOM, nMP, nPM, parent_session,
                                 window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          column(12,
                 shinycssloaders::withSpinner(uiOutput(ns('kobetimeplots')))
          )
        )
      )
    })

    make_plots <- reactive({
      slick <- filtered_slick()
      if (is.null(slick))
        return(NULL)
      dd <- slick |> Kobe() |> Value() |>  dim()
      plot_list <- list()
      if (dd[3]==nMP()) {
        for (i in 1:nMP()) {

          plot_list[[i]] <- Kobe_time_plot(slick, i)
        }
      }
      plot_list
    })


    output$kobetimeplots <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nMP(), function(mm) {
          plotname <- paste("plot", mm, sep="")
          shinycssloaders::withSpinner(plotOutput(ns(plotname), width='300px', height='300px'))
        })
        plot_output_list$cellArgs=list(style = 'width: 320px;')
        do.call(flowLayout, plot_output_list)
      }
    })

    observeEvent(make_plots(), {
      thisplot <- make_plots()
      for (i in 1:nMP()) {
        local({
          my_i <- i
          plotname <- paste("plot", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]]
          }, height=300, width=300)
        })
      }
    })




  })
}

## To be copied in the UI
# mod_Kobe_time_ui("Kobe_time_1")

## To be copied in the server
# mod_Kobe_time_server("Kobe_time_1")

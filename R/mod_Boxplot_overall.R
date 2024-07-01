#' Boxplot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Boxplot_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('results'))
  )
}

#' Boxplot_overall Server Functions
#'
#' @noRd
mod_Boxplot_overall_server <- function(id, i18n, filtered_slick,
                                       plottype,
                                       nOM, nMP, nPM, parent_session,
                                       window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    plot_width_calc <- reactive({
      width <- nMP()*50
      width <- max(width, 200)
      paste0(width, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    plot_width_text <- reactive({
      paste0('width: ', plot_width(), '; height: 320px;')

    })

    output$selectedtype <- reactive({
      plottype()
    })

    outputOptions(output, "selectedtype", suspendWhenHidden = FALSE)

    output$results <- renderUI({
      tagList(
        conditionalPanel("output.selectedtype=='1'", ns=ns,
                        uiOutput(ns('boxplots'))
        ),
        conditionalPanel("output.selectedtype=='2'", ns=ns,
                         uiOutput(ns('violins'))
        ),
        conditionalPanel("output.selectedtype=='3'", ns=ns,
                         uiOutput(ns('both'))
        )
      )
    })


    output$boxplots <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("boxplot", mm, sep="")
          loading_spinner(plotOutput(session$ns(plotname),
                                                  width=plot_width(),
                                                  height='300px'))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    output$violins <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("violin", mm, sep="")
          loading_spinner(plotOutput(session$ns(plotname), width=plot_width(),
                                                  height='300px'))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    output$both <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("both", mm, sep="")
          loading_spinner(plotOutput(session$ns(plotname), width=plot_width(),
                                                  height='300px'))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })


    make_plots <- reactive({

      req(filtered_slick())
      if (is.null(filtered_slick())) {
        return(NULL)
      }
      if (is.na(nPM()))
        return(NULL)
      dd <- filtered_slick() |> Boxplot() |> Value() |>
        dim()

      plot_list <- list()
      if (dd[4]==nPM()) {
        for (i in 1:nPM()) {
          plot_list[[i]] <- plotBoxplot(filtered_slick(), i, 'all', FALSE, FALSE)
        }
      }
      plot_list
    })


    observeEvent(make_plots(), {
      thisplot <- make_plots()
      for (i in 1:nPM()) {
        local({
          my_i <- i
          plotname <- paste("boxplot", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[1]]
          })

          plotname <- paste("violin", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[2]]
          })

          plotname <- paste("both", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[3]]
          })
        })
      }
    })


  })
}





## To be copied in the UI
# mod_Boxplot_overall_ui("Boxplot_overall_1")

## To be copied in the server
# mod_Boxplot_overall_server("Boxplot_overall_1")

#' Timeseries_byOM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Timeseries_byOM_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('selections')),
    uiOutput(ns('page'))
  )
}

#' Timeseries_byOM Server Functions
#'
#' @noRd
mod_Timeseries_byOM_server <- function(id, i18n, filtered_slick,
                                       pm_ind, yrange, nOM,
                                       window_dims,
                                       selected_oms,
                                       Report, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    timeseries <- reactiveVal()

    observeEvent(filtered_slick(),
                 timeseries(filtered_slick()))

    values <- reactive({
      filtered_slick() |>
        Timeseries() |>
        Value()

    })

    sims <- reactive({
      vals <-values()
      1:dim(vals)[1]
    })

    output$selections <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          column(3,
                 radioButtons(ns('plotoption'),
                              i18n$t('Plot Option'),
                              choiceNames = i18n$t(c('Median', 'Individual Simulation')),
                              choiceValues= c('median', 'sim')
                 )
          ),
          column(6,
                 conditionalPanel("input.plotoption=='sim'", ns=ns,
                                  column(6,
                                         shinyWidgets::pickerInput(
                                           inputId = ns('selectsim'),
                                           label = i18n$t('Individual Simulation:'),
                                           choices = sims(),
                                           multiple = FALSE,
                                           width='fit'
                                         )
                                  ),
                                  column(6,
                                         shinyjs::hidden(
                                           shinyWidgets::actionBttn(ns('updateplot'),
                                                                    i18n$t('Update Plots'))
                                         )
                                  )

                 )
          )
        )

      )
    })



    observeEvent(input$plotoption, {
      if (input$plotoption != 'sim') {
        shinyjs::hide('updateplot')
        timeseries(filtered_slick())
      } else {
        shinyjs::show('updateplot')
      }

    })

    observeEvent(input$selectsim, {
      shinyjs::show('updateplot')
    })

    observeEvent(input$updateplot, {
      shinyjs::hide('updateplot')
      slick <- filtered_slick()
      sim <- as.numeric(input$selectsim)
      slick@Timeseries@Value <- slick@Timeseries@Value[sim,,,,,drop=FALSE]
      timeseries(slick)
    }, ignoreInit = TRUE)


    output$page <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nOM(), function(mm) {
          plotname <- paste("plot", mm, sep="")
          loading_spinner(plotOutput(session$ns(plotname), width=plot_width(),
                                                  height=plot_height()))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- (dd[1] * 0.2) |> round(0)
      paste0(val, 'px')
    })

    plot_height_calc <- reactive({
      dd <- window_dims()
      val <- (dd[1] * 0.2) |> round(0)
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    plot_height <- plot_height_calc |> debounce(500)

    plot_width_text <- reactive({
      paste0('width: ', plot_width(), '; height: ', plot_height())
    })

    make_plots <- reactive({
      if (is.null(timeseries()))
        return(NULL)
      if (is.null(pm_ind()))
        return(NULL)
      if (is.null(yrange()))
        return(NULL)
      dd <- timeseries() |> Timeseries() |> Value() |>  dim()
      plot_list <- list()
      if (dd[2]==nOM()) {
        om_labels <- selected_oms()
        for (i in 1:nOM()) {
          plot_list[[i]] <- plotTimeseries(timeseries(),
                                            pm_ind(),
                                            MP_ind=NULL,
                                            i,
                                           OM_label = om_labels[i])
        }
      }
      plot_list
    })


    observeEvent(make_plots(), {
      thisplot <- make_plots()
      if (length(thisplot)>0) {
        for (i in 1:nOM()) {
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")
            output[[plotname]] <- renderPlot({
              thisplot[[my_i]] +
                ggplot2::coord_cartesian(ylim=yrange())
            })
          })
        }
      }
    })


  })
}

## To be copied in the UI
# mod_Timeseries_byOM_ui("Timeseries_byOM_1")

## To be copied in the server
# mod_Timeseries_byOM_server("Timeseries_byOM_1")

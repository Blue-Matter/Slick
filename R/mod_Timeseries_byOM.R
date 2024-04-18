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
    div(class='top_border',
        column(2,
               br(),
               uiOutput(ns('MPlist')),
               uiOutput(ns('selections'))
               ),
        column(10,
               uiOutput(ns('page'))
               )

    )
  )
}

#' Timeseries_byOM Server Functions
#'
#' @noRd
mod_Timeseries_byOM_server <- function(id, i18n, filtered_slick,
                                       pm_ind, yrange, nOM,
                                       window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
      shinyWidgets::pickerInput(
        inputId = ns('selectsim'),
        label = i18n$t('Individual Simulation(s):'),
        choices = sims(),
        multiple = TRUE,
        width='fit'
      )
    })


    output$page <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nOM(), function(mm) {
          plotname <- paste("plot", mm, sep="")
          shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width=plot_width(),
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
      if (is.null(filtered_slick()))
        return(NULL)
      if (is.null(pm_ind()))
        return(NULL)
      if (is.null(yrange()))
        return(NULL)
      dd <- filtered_slick() |> Timeseries() |> Value() |>  dim()
      plot_list <- list()
      if (dd[2]==nOM()) {

        for (i in 1:nOM()) {
          plot_list[[i]] <- Timeseries_plot(filtered_slick(),
                                            pm_ind(),
                                            yrange(),
                                            mp_ind=NULL,
                                            i,
                                            sims=NULL)
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
              thisplot[[my_i]]
            })
          })
        }
      }
    })

    output$MPlist <- renderUI({
      i18n <- i18n()
      mp_metadata <- filtered_slick() |> MPs() |> Metadata()
      MPcols <- mp_metadata$Color
      MPnames <- mp_metadata$Label

      text <- paste0("<p> <b class='horizline' style=' border-top: .3rem solid ", MPcols, ";'></b>",
                     MPnames, "</p>")
      text <- paste(text, collapse=" ")
      tagList(
        strong(i18n$t("Management Procedure")),
        HTML(text)
      )
    })

  })
}

## To be copied in the UI
# mod_Timeseries_byOM_ui("Timeseries_byOM_1")

## To be copied in the server
# mod_Timeseries_byOM_server("Timeseries_byOM_1")

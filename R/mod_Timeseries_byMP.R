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
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('page'))
  )
}

#' Timeseries_byMP Server Functions
#'
#' @noRd
mod_Timeseries_byMP_server <- function(id, i18n, filtered_slick,
                                       pm_ind, yrange, nMP,
                                       window_dims,
                                       Report, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Timeseries',
                          window_dims)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {
      Plot_Object(timeseriesplot()+ ggplot2::coord_cartesian(ylim=yrange()))

      if(!inherits(Plot_Object(), 'NULL'))
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    })



    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.6
      paste0(val, 'px')
    })

    plot_height_calc <- reactive({
      nmp <- nMP()
      ncol <- min(nmp, 4)

      nrow <- ceiling(nmp/ncol)

      paste0(nrow*300, 'px')

    })

    plot_width <- plot_width_calc |> debounce(500)
    plot_height <- plot_height_calc |> debounce(500)


    output$page <- renderUI({
      loading_spinner(plotOutput(ns('timeseriesMP'),
                                 width=plot_width(),
                                 height=plot_height()))
    })

    timeseriesplot <- reactive({
      if (is.null(filtered_slick()))
        return(NULL)
      if (is.null(pm_ind()))
        return(NULL)
      if (is.null(yrange()))
        return(NULL)
      plotTimeseries(filtered_slick(), pm_ind(),
                     MP_ind=1:nMP(),
                     includeHist=TRUE) +
        ggplot2::coord_cartesian(ylim=yrange())
    })

    output$timeseriesMP <- renderPlot({
      timeseriesplot()
    })


    plot_width_text <- reactive({
      paste0('width: ', '400px;', '; height: 300px;')
    })

    #############################################################################
    # output$page <- renderUI({
    #   if (!is.null(make_plots())) {
    #     plot_output_list <- lapply(1:nMP(), function(mm) {
    #       plotname <- paste("plot", mm, sep="")
    #       loading_spinner(plotOutput(session$ns(plotname), width='400px',
    #                                               height='300px'))
    #     })
    #     plot_output_list$cellArgs=list(style = plot_width_text())
    #     do.call(flowLayout, plot_output_list)
    #   }
    # })


    # plot_width_text <- reactive({
    #   paste0('width: ', '400px;', '; height: 300px;')
    # })
#
#     make_plots <- reactive({
#       if (is.null(filtered_slick()))
#         return(NULL)
#       if (is.null(pm_ind()))
#         return(NULL)
#       if (is.null(yrange()))
#         return(NULL)
#       dd <- filtered_slick() |> Timeseries() |> Value() |>  dim()
#       plot_list <- list()
#       if (dd[3]==nMP()) {
#         for (i in 1:nMP()) {
#           plot_list[[i]] <- plotTimeseries(filtered_slick(), pm_ind(),
#                                             MP_ind=i,
#                                            includeHist=FALSE)
#         }
#       }
#       plot_list
#     })
#
#
#     observeEvent(make_plots(), {
#       thisplot <- make_plots()
#       if (length(thisplot)>0) {
#         for (i in 1:nMP()) {
#           local({
#             my_i <- i
#             plotname <- paste("plot", my_i, sep="")
#             output[[plotname]] <- renderPlot({
#               thisplot[[my_i]] +
#                 ggplot2::coord_cartesian(ylim=yrange())
#             })
#           })
#         }
#       }
#     })



  })
}

## To be copied in the UI
# mod_Timeseries_byMP_ui("Timeseries_byMP_1")

## To be copied in the server
# mod_Timeseries_byMP_server("Timeseries_byMP_1")


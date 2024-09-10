#' Report_Page_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Report_Page_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('showplot'))
  )
}

#' Report_Page_plot Server Functions
#'
#' @noRd
mod_Report_Page_plot_server <- function(id, PlotName='Boxplot', Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(Report[[PlotName]], {
      nplot <- length(Report[[PlotName]]$plot)
      if (nplot>0) {
        for (x in 1:nplot) {
          local({
            this_x <- x
            this_plot <- Report[[PlotName]]$plot[[this_x]]
            if (!prod(is.na(this_plot)))
              output[[paste(this_x, tolower(PlotName), sep="")]] <- renderImage({
                this_plot
              }, deleteFile=FALSE)
          })
        }
      }
    })

    observe({
      nplot <- length(Report[[PlotName]]$plot)
      if (nplot>0) {
        for (x in 1:nplot) {
          this_x <- x
          observeEvent(eventExpr = input[[paste0('del-', this_x, tolower(PlotName))]],
                       handlerExpr = {
                         if (!all(is.na(Report[[PlotName]]$plot[[this_x]])))
                           file.remove(Report[[PlotName]]$plot[[this_x]]$src)
                         Report[[PlotName]]$plot[[this_x]] <- NA
                         Report[[PlotName]]$caption[[this_x]] <- NA
                       })
        }
      }
    })

    plotlist <- reactive({
      nplot <- length(Report[[PlotName]]$plot)
      if (nplot>0) {
        plot_output_list <- lapply(1:nplot, function(x) {
          plotname <- paste0(x, tolower(PlotName))
          caption <- Report[[PlotName]]$caption[[x]]
          if (!is.na(caption)) {
            caption <- p(caption)
          } else {
            caption <- NULL
          }
          if (!is.null(caption))
            tagList(
              hr(),
              imageOutput(ns(plotname)),
              caption,
              shinyWidgets::actionBttn(ns(paste0('del-', plotname)),
                                       label='Remove',
                                       icon('remove'),
                                       color='danger',size='sm'),
              hr()
            )
        })
        do.call('tagList', plot_output_list)
      }
    })

    output$showplot <- renderUI({
      chk <- lapply(Report[[PlotName]]$plot, is.na) |> unlist()
      if (!all(chk)) {
        tagList(
          h3(PlotName),
          plotlist()
        )
      }
    })
  })
}

## To be copied in the UI
# mod_Report_Page_plot_ui("Report_Page_plot_1")

## To be copied in the server
# mod_Report_Page_plot_server("Report_Page_plot_1")

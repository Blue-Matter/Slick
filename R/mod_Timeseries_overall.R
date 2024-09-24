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
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('plot'))
  )
}

#' Timeseries_overall Server Functions
#'
#' @noRd
mod_Timeseries_overall_server <- function(id, i18n, filtered_slick,
                                          pm_ind, yrange,
                                          window_dims, Report, parent_session,
                                          includeQuants, includeLabels, includeHist
                                          ){
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

    timeseriesplot <- reactive({
      req(pm_ind())
      plotTimeseries(filtered_slick(), pm_ind(),
                     includeQuants =includeQuants(),
                     includeLabels =includeLabels(),
                     includeHist = includeHist())
    })

    output$plot1 <- renderPlot({
       p <- timeseriesplot()
       p + ggplot2::coord_cartesian(ylim=yrange())

    })

    output$plot <- renderUI({
      tagList(
        br(),
        loading_spinner(
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

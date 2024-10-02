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
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('overall_Spider'))
  )
}

#' Spider_overall Server Functions
#'
#' @noRd
mod_Spider_overall_server <- function(id, i18n, filtered_slick,
                                      nOM, nMP, nPM,
                                      relative_scale=relative_scale,
                                      window_dims,
                                      Report,
                                      parent_session){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Spider',
                          window_dims)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {
      LL <- list()
      LL$plotFunction <- plotSpider
      LL$slick <- filtered_slick()
      LL$relScale <- relative_scale()
      Plot_Object(LL)
      if(!inherits(Plot_Object(), 'NULL')) {
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
      }
    })


    output$overall_Spider <- renderUI({
      i18n <- i18n()
      tagList(
        loading_spinner(
          plotOutput(ns('spider_plot'), height=plot_height(),
                     width=plot_width())
        )
      )
    })

    plot_height_calc <- reactive({
      dims <- window_dims()
      dims[1]*0.4
    })

    plot_width_calc <- reactive({
      dims <- window_dims()
      dims[1]*0.4
    })

    plot_height <- plot_height_calc |> debounce(500)

    plot_width <- plot_width_calc |> debounce(500)

    spiderPlot <- reactive({
      req(relative_scale)
      req(filtered_slick)
      plotSpider(filtered_slick(), relScale=relative_scale())

    })

    output$spider_plot <- renderPlot({
      if (!is.null(relative_scale()))
        spiderPlot()
    }, width=function() {
      plot_width()
    }, height=function() {
      plot_height()
    })

  })
}

## To be copied in the UI
# mod_Spider_overall_ui("Spider_overall_1")

## To be copied in the server
# mod_Spider_overall_server("Spider_overall_1")

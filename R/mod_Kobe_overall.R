#' Kobe_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Kobe_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('page'))
  )
}

#' Kobe_overall Server Functions
#'
#' @noRd
mod_Kobe_overall_server <- function(id, i18n, filtered_slick,
                                    plottype,
                                    nOM, nMP, nPM, parent_session,
                                    window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          column(3,
                 h4(strong(i18n$t("Reading this Chart"))),
                 htmlOutput(ns('reading'))
          ),
          column(7,
                 shinycssloaders::withSpinner(plotOutput(ns('results'),
                                                         height=plot_height(),
                                                         width=plot_width()))
          ),
          column(2,
                 uiOutput(ns('controls')))
        )
      )
    })

    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.4
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)
    plot_height <- plot_width_calc |> debounce(500)

    output$results <- renderPlot({
      req(filtered_slick())
      req(input$xaxis)
      req(input$yaxis)
      slick <- filtered_slick()

      Kobe_plot(slick,
                xvar=1, yvar=2,
                ts=NA,
                xmax=x_axis(),
                ymax=y_axis(),
                inc_line=input$histline,
                selected_quantile())
    })

    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

    output$controls <- renderUI({
      i18n <- i18n()
      tagList(
        h4(i18n$t('Select Percentiles')),
        sliderInput(ns('selectquant'),
                    i18n$t('Percentile'),
                    0,
                    1,
                    0.75,
                    step=0.05
        ),
        h4(i18n$t('Set Maximum Axis Value')),
        numericInput(ns('xaxis'),
                     'X Axis',
                     2,
                     0,
                     step=0.25),
        numericInput(ns('yaxis'),
                     'Y Axis',
                     2,
                     0,
                     step=0.25),
        checkboxInput(ns('histline'),
                      i18n$t('Include historical line?'),
                      value=TRUE)
      )
    })

    selected_quantile <- reactive({
      req(input$selectquant)
      input$selectquant
    })

    x_axis_val <- reactive({
      input$xaxis
    })

    y_axis_val <- reactive({
      input$yaxis
    })

    x_axis <- x_axis_val |> debounce(300)
    y_axis <- y_axis_val |> debounce(300)

    output$reading <- renderUI({
      i18n <- i18n()
      slick <- filtered_slick()
      mp_metadata <-slick |> MPs() |> Metadata()
      time_info <- slick |> Kobe() |> Time()
      yrs <- time_info[[1]]
      quant_text <- selected_quantile() * 100

      yr.txt <- paste0(min(yrs), '-', max(yrs))
      tagList(
        p(i18n$t('This chart compares trade-offs in'), nMP(),
          i18n$t(' management procedures for '), nOM(),
          i18n$t(' operating models by measuring two co-dependent performance metrics: fishing mortality (vertical axis) and biomass (horizontal axis)')
        ),
        p(
          HTML('<i class="fas fa-circle fa-sm"></i>'),
          i18n$t('The large colored dots represent the median value for the final year of the projection period: '),
          yr.txt),
        p(i18n$t('The small colored dots indicate the median value in the first year of the projections, and the colored lines show how the median values change over time.')
          ),
        p( i18n$t('The white dotted lines around dots are error bars. The default represents'),
           HTML(paste0(quant_text, 'th')),
           i18n$t('percentiles, but that can be changed using the "Select Percentiles" scale at the right.')
        ),
        p(i18n$t('Use the'), actionLink(ns('openfilter'), i18n$t('Filter'), icon=icon('fa-lg fa-filter', class='fa-regular')),
          i18n$t('button to filter the Management Procedures and Operating Models included in the plot.')
        )
      )
    })


  })
}

## To be copied in the UI
# mod_Kobe_overall_ui("Kobe_overall_1")

## To be copied in the server
# mod_Kobe_overall_server("Kobe_overall_1")

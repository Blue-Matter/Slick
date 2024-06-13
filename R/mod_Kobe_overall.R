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
                                    xvar, yvar,
                                    window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$page <- renderUI({
      req(filtered_slick())
      chk <- Check(filtered_slick())
      if (chk@empty$Kobe) return(NULL)

      i18n <- i18n()
      tagList(
        fluidRow(
          column(10,
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
      # req(filtered_slick())
      # req(input$xaxis)
      # req(input$yaxis)
      slick <- filtered_slick()

      Kobe_plot(slick,
                xvar=xvar(), yvar=yvar(),
                ts=NA,
                xmax=x_axis(),
                ymax=y_axis(),
                inc_line=input$histline,
                selected_quantile())
    })


    output$controls <- renderUI({
      i18n <- i18n()
      tagList(
        h4(i18n$t('Error Bars')),
        checkboxInput(ns('show_percentiles'),
                      i18n$t('Include Error Bars?'),
                      value=TRUE),
        conditionalPanel("output.showerrorbars", ns=ns,
                         sliderInput(ns('selectquant'),
                                     i18n$t('Percentile'),
                                     0,
                                     1,
                                     0.75,
                                     step=0.05
                                     )
                         ),
        h4(i18n$t('Set Maximum Axis Value')),
        numericInput(ns('xaxis'),
                     'X Axis',
                     initX(),
                     0,
                     step=0.25),
        numericInput(ns('yaxis'),
                     'Y Axis',
                     initY(),
                     0,
                     step=0.25),
        checkboxInput(ns('histline'),
                      i18n$t('Show line for entire projection period?'),
                      value=FALSE)
      )
    })

    output$showerrorbars <- reactive({
      input$show_percentiles==TRUE
    })
    outputOptions(output, "showerrorbars", suspendWhenHidden = FALSE)

    selected_quantile <- reactive({
      req(input$selectquant)
      if (input$show_percentiles == FALSE)
        return(0)
      input$selectquant
    })

    initX <- reactive({
      slick <- filtered_slick()
      if (is.null(slick)) return(2)
      kobe <- Kobe(slick)
      val <- max(apply(Value(kobe)[,,,1,, drop=FALSE], c(3,5), median, na.rm=TRUE))
      ceiling(val)
    })

    initY <- reactive({
      slick <- filtered_slick()
      if (is.null(slick)) return(2)
      kobe <- Kobe(slick)
      val <- max(apply(Value(kobe)[,,,2,, drop=FALSE], c(3,5), median, na.rm=TRUE))
      ceiling(val)
    })

    x_axis_val <- reactive({
      input$xaxis
    })

    y_axis_val <- reactive({
      input$yaxis
    })

    x_axis <- x_axis_val |> debounce(500)
    y_axis <- y_axis_val |> debounce(500)


    selected_quantile

  })
}

## To be copied in the UI
# mod_Kobe_overall_ui("Kobe_overall_1")

## To be copied in the server
# mod_Kobe_overall_server("Kobe_overall_1")

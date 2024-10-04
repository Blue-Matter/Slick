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
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('page'))
  )
}

#' Kobe_overall Server Functions
#'
#' @noRd
mod_Kobe_overall_server <- function(id, i18n,
                                    filtered_slick,
                                    parent_session,
                                    xvar, yvar,
                                    window_dims,
                                    Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Kobe',
                          window_dims)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {
      Plot_Object(kobeplot())

      if(!inherits(Plot_Object(), 'NULL'))
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    })

    kobeplot <- reactive({
      plotKobe(filtered_slick(),
               xPI=xvar(),
               yPI=yvar(),
               TS=NA,
               xmax=x_axis(),
               ymax=y_axis(),
               hist_traj =input$histline,
               percentile=selected_quantile())
    })


    nOM <- reactive({
      slick <- filtered_slick()
      if (!is.null(slick))
        nrow(slick@OMs@Design)
    })

    nMP <- reactive({
      slick <- filtered_slick()
      if (!is.null(slick))
        length(slick@MPs@Code)
    })

    nPM <- reactive({
      slick <- filtered_slick()
      if (!is.null(slick))
        length(slick@Kobe@Code)
    })

    output$page <- renderUI({
      req(filtered_slick())
      chk <- Check(filtered_slick())
      if (chk@empty$Kobe) return(NULL)

      i18n <- i18n()
      tagList(
        fluidRow(
          column(10,
                 loading_spinner(plotOutput(ns('results'),
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
      val <- dd[1] * 0.3
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)
    plot_height <- plot_width_calc |> debounce(500)

    output$results <- renderPlot({
      kobeplot()
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
      mean_over_OMs <- apply(Value(kobe), c(1,3,4,5), mean, na.rm=TRUE)
      val <- max(apply(mean_over_OMs[,,1,,drop=FALSE], c(2,4), median, na.rm=TRUE))
      ceiling(val)
    })

    initY <- reactive({
      slick <- filtered_slick()
      if (is.null(slick)) return(2)
      kobe <- Kobe(slick)
      mean_over_OMs <- apply(Value(kobe), c(1,3,4,5), mean, na.rm=TRUE)
      val <- max(apply(mean_over_OMs[,,2,,drop=FALSE], c(2,4), median, na.rm=TRUE))
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

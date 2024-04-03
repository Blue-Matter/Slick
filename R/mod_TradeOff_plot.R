

#' TradeOff_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TradeOff_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot'))
  )
}

#' TradeOff_plot Server Functions
#'
#' @noRd
mod_TradeOff_plot_server <- function(id, i18n, Slick_Object, Filter_Selected, parent_session, window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_quilt <- reactive({
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      selected_MPs <- Filter_Selected$MPs
      quilt <- Quilt(slick)

      dd <- dim(Value(quilt))
      if (length(selected_OMs)==dd[1]) {
        # filter OMs
        if (!is.null(selected_OMs)) {
          Value(quilt) <- Value(quilt)[selected_OMs,,, drop=FALSE]
        }
        # filter MPs
        if (!is.null(selected_MPs)) {
          Value(quilt) <- Value(quilt)[,selected_MPs,, drop=FALSE]
        }
      }
      quilt
    })

    nOM <- reactive({
      dim(Value(filtered_quilt()))[1]
    })

    filtered_MPs <- reactive({
      slick <- Slick_Object()
      Metadata(MPs(slick))[Filter_Selected$MPs,]
    })

    nMP <- reactive({
      nrow(filtered_MPs())
    })

    pm_metadata <- reactive({
      Metadata(Quilt(Slick_Object()))
    })

    PM_codes <- reactive({
      pm_metadata()[['Code']]
    })

    output$plot <- renderUI({
      i18n <- i18n()
      quilt <- filtered_quilt()
      pm_codes <- PM_codes()
      tagList(
        br(),
        # style='padding: 10px;',
        shinydashboard::box(width=12,
                            collapsible = TRUE,
                            status='primary',
                            title=strong(i18n$t("READING THIS CHART")),
                            uiOutput(ns('reading'))
        ),
        shinydashboard::box(width=12,
                            status='primary',
                            title=strong(i18n$t('Trade-Off Plot')),
                            column(3, uiOutput(ns('pmselection'))),
                            column(9,uiOutput(ns('tradeoff')))
        )
      )
    })

    output$reading <- renderUI({
      i18n <- i18n()
      tagList(
        column(6,
               p('This chart plots the tradeoffs between two performance indicators for ',
                                      nMP(), ' management procedures (MP). ...')
        ),
        column(6,
               p(i18n$t('Use the'), actionLink(ns('openfilter'), i18n$t('Filter'), icon=icon('filter')),
                 i18n$t('button to filter the Management Procedures and Operating Models used in this plot.  ...')
               )
        )
      )
    })

    output$pmselection <- renderUI({
      i18n <- i18n()
      pm_codes <- PM_codes()
      tagList(
        p(i18n$t('Select the Performance Indicators to show on the X and Y axes of the Trade-Off plot:')),
        shinyWidgets::pickerInput(
          inputId = ns('xPM'),
          label = i18n$t("X-Axis Performance Indicator"),
          selected=pm_codes[1],
          choices = pm_codes
        ),
        shinyWidgets::pickerInput(
          inputId = ns('yPM'),
          label = i18n$t("Y-Axis Performance Indicator"),
          selected=pm_codes[2],
          choices = pm_codes
        )
      )
    })

    output$tradeoff <- renderUI({
      i18n <- i18n()
      tagList(
        h4(strong(paste(nMP(),
                        i18n$t('Management Procedures. Median values over'),
                        nOM(),
                        i18n$t('Operating Models'))
        )),
        plotOutput(ns('tradeoffplot'), height=plot_height_d())
      )

    })

    plot_height <- reactive({
      dims <- window_dims()
      dims[1]*0.3
    })
    plot_height_d <- plot_height |> debounce(500)

    output$tradeoffplot <- renderPlot({
      plotTradeoff(filtered_quilt(), filtered_MPs(), input$xPM, input$yPM)
    }, width=function() {
      plot_height_d()
    }, height=function() {
      plot_height_d()
    })


    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

  })
}

## To be copied in the UI
# mod_TradeOff_plot_ui("TradeOff_plot_1")

## To be copied in the server
# mod_TradeOff_plot_server("TradeOff_plot_1")

#' PM_Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PM_Info_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('PMinfo'))
  )
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' PM_Info Server Functions
#'
#' @noRd
mod_PM_Info_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    PITabs <- reactive({
      slick <- Slick_Object()
      i18n <- i18n()

      ll <- list()
      PIplots <- c('Boxplot', 'Kobe', 'Quilt', 'Spider', 'Time Series', 'Tradeoff')
      for (i in seq_along(PIplots)) {
        slotname <- PIplots[i]
        slotname <- firstup(gsub(' ' ,'', tolower(slotname)))
        tab_name <- paste('PM', slotname, sep='_')

        if (!is_empty(slot(slick, slotname)))
          ll[[i]] <- tabPanel(PIplots[i],
                              br(),
                              DT::dataTableOutput(ns(tab_name)))
      }
      ll
    })


    output$PMinfo <- renderUI({
      # i18n <- i18n()
      # tabsetPanel(type = "tabs", id='pmtabsetpanel',
      #             tabPanel("Boxplot",
      #                      br(),
      #                      DT::dataTableOutput(session$ns('PM_Boxplot'))),
      #             tabPanel("Kobe",
      #                      br(),
      #                      DT::dataTableOutput(session$ns('PM_Kobe'))),
      #             tabPanel("Quilt",
      #                      br(),
      #                      DT::dataTableOutput(session$ns('PM_Quilt'))),
      #             tabPanel("Spider",
      #                      br(),
      #                      DT::dataTableOutput(session$ns('PM_Spider'))),
      #             tabPanel("Time Series",
      #                      br(),
      #                      DT::dataTableOutput(session$ns('PM_Timeseries'))),
      #             tabPanel("Tradeoff",
      #                      br(),
      #                      DT::dataTableOutput(session$ns('PM_Tradeoff')))
      # )
      do.call(tabsetPanel, c(PITabs(), list(id='pmtabsetpanel')))
    })

    output$PM_Boxplot <- DT::renderDataTable({
      tableBoxplot(Boxplot(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Kobe <- DT::renderDataTable({
      tableKobe(Kobe(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Quilt <- DT::renderDataTable({
      tableQuilt(Quilt(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Spider <- DT::renderDataTable({
      tableSpider(Spider(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Timeseries <- DT::renderDataTable({
      tableTimeseries(Timeseries(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Tradeoff <- DT::renderDataTable({
      tableTradeoff(Tradeoff(Slick_Object()), i18n()$get_translation_language())
    })


  })
}


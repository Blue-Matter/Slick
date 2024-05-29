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

#' PM_Info Server Functions
#'
#' @noRd
mod_PM_Info_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$PMinfo <- renderUI({
      i18n <- i18n()
      tabsetPanel(type = "tabs", id='pmtabsetpanel',
                  tabPanel(i18n$t("Boxplot"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Boxplot'))),
                  tabPanel(i18n$t("Kobe"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Kobe'))),
                  tabPanel(i18n$t("Quilt"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Quilt'))),
                  tabPanel(i18n$t("Spider"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Spider'))),
                  tabPanel(i18n$t("Timeseries"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Timeseries'))),
                  tabPanel(i18n$t("Tradeoff"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Tradeoff')))
      )
    })

    output$PM_Boxplot <- DT::renderDataTable({
      Table(Boxplot(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Kobe <- DT::renderDataTable({
      Table(Kobe(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Quilt <- DT::renderDataTable({
      Table(Quilt(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Spider <- DT::renderDataTable({
      Table(Spider(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Timeseries <- DT::renderDataTable({
      Table(Timeseries(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Tradeoff <- DT::renderDataTable({
      Table(Tradeoff(Slick_Object()), i18n()$get_translation_language())
    })


  })
}


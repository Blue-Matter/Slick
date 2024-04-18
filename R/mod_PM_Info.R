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
      tabsetPanel(type = "tabs",
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


    output$PM_Quilt <- DT::renderDataTable({
      tableQuilt(Quilt(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Spider <- DT::renderDataTable({
      tableSpider(Spider(Slick_Object()), i18n()$get_translation_language())
    })

    output$PM_Boxplot <- DT::renderDataTable({
      tableBoxplot(Boxplot(Slick_Object()), i18n()$get_translation_language())
    })

  })
}


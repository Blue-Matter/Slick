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
                  tabPanel(i18n$t("Quilt"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Quilt'))),
                  tabPanel(i18n$t("Spider"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Spider'))),
                  tabPanel(i18n$t("Boxplot"),
                           br(),
                           DT::dataTableOutput(session$ns('PM_Boxplot')))
      )
    })


    output$PM_Quilt <- DT::renderDataTable({
      i18n <- i18n()
      slick <- Slick_Object()
      obj <- Quilt(slick)

      df <- data.frame(Label=Label(obj, i18n$get_translation_language()),
                 Description=Description(obj, i18n$get_translation_language())
      )
      DT::datatable(df,
                    extensions = 'Responsive',
                    selection='none',
                    options = list(dom = 't',
                                   pageLength=100,
                                   ordering=F))
    })

    output$PM_Spider <- DT::renderDataTable({
      i18n <- i18n()
      slick <- Slick_Object()
      obj <- Spider(slick)

      df <- data.frame(Label=Label(obj, i18n$get_translation_language()),
                       Description=Description(obj, i18n$get_translation_language())
      )
      DT::datatable(df,
                    extensions = 'Responsive',
                    selection='none',
                    options = list(dom = 't',
                                   pageLength=100,
                                   ordering=F))
    })

    output$PM_Boxplot <- DT::renderDataTable({
      i18n <- i18n()
      slick <- Slick_Object()
      obj <- Boxplot(slick)

      df <- data.frame(Label=Label(obj, i18n$get_translation_language()),
                       Description=Description(obj, i18n$get_translation_language())
      )
      DT::datatable(df,
                    extensions = 'Responsive',
                    selection='none',
                    options = list(dom = 't',
                                   pageLength=100,
                                   ordering=F))
    })

  })
}

## To be copied in the UI
# mod_PM_Info_ui("PM_Info_1")

## To be copied in the server
# mod_PM_Info_server("PM_Info_1")

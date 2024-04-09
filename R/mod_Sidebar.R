#' Sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::sidebarMenuOutput(ns('sidebar'))
  )
}

#' Sidebar Server Functions
#'
#' @noRd
mod_Sidebar_server <- function(id, i18n, Load_Slick_File){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    menu_list <- reactive({
      i18n <- i18n()
      ll <- list(shinydashboard::menuItem(i18n$t('Home'),
                                          tabName='hometab',
                                          icon=icon('home')
                                          )
                 )
      # fix a bug in icon
      info_circle <- icon("info-circle")
      info_circle$attribs$class <-  "fas fa-info-circle"

      if (Load_Slick_File$loaded >=1) {
        ll <- list(shinydashboard::menuItem(i18n$t('Home'), tabName='hometab',
                                            icon=icon('home')),
                   shinydashboard::menuItem(i18n$t("Overview"),
                                            tabName = "metadatatab",
                                            icon = info_circle
                                            ),
                   shinydashboard::menuItem(i18n$t("Quilt"),
                                            tabName = "quilt",
                                            icon = icon("table")),
                   shinydashboard::menuItem(i18n$t("Spider"),
                                            tabName = "spider",
                                            icon = icon("circle")),
                   shinydashboard::menuItem(i18n$t("Boxplot"),
                                            tabName = "boxplot",
                                            icon = icon("square")),
                   shinydashboard::menuItem(i18n$t("Kobe"),
                                            tabName = "kobe",
                                            icon = icon("border-all")),
                   shinydashboard::menuItem(i18n$t("Line"),
                                            tabName = "line",
                                            icon = icon("chart-line")),
                   shinydashboard::menuItem(i18n$t("Report"),
                                            tabName = "report",
                                            icon = icon("file"))
        )
      }
      ll
    })

    output$sidebar <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(id='sidebarmenu', .list=menu_list())
    })
  })
}

## To be copied in the UI
# mod_Sidebar_ui("Sidebar_1")

## To be copied in the server
# mod_Sidebar_server("Sidebar_1")

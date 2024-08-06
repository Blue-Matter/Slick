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
mod_Sidebar_server <- function(id, i18n, Load_Slick_File, Slick_Object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    menu_list <- reactive({
      i18n <- i18n()
      ll <- list()
      ll[[1]] <- list(shinydashboard::menuItem(i18n$t('Home'),
                                               tabName='hometab',
                                               icon=icon('home')))

      ll[[2]] <- list(shinydashboard::menuItem(i18n$t('Home'), tabName='hometab',
                                               icon=icon('house')),

                      shinydashboard::menuItem(i18n$t("Overview"),
                                               tabName = "metadatatab",
                                               icon = icon('info-circle')
                      ),

                      shinydashboard::menuItem("Time Series",
                                               tabName = "timeseries",
                                               icon = icon("chart-line-up-down")),

                      shinydashboard::menuItem("Boxplot",
                                               tabName = "boxplot",
                                               icon = icon("fa-regular fa-chart-candlestick")),

                      shinydashboard::menuItem("Kobe",
                                               tabName = "kobe",
                                               icon = icon("table-cells-large")),

                      shinydashboard::menuItem("Quilt",
                                               tabName = "quilt",
                                               icon = icon("table-cells")),

                      shinydashboard::menuItem("Spider",
                                               tabName = "spider",
                                               icon = icon("fa-hexagon", class='fas')),

                      shinydashboard::menuItem("Tradeoff",
                                               tabName = "tradeoff",
                                               icon = icon('chart-scatter')),

                      shinydashboard::menuItem(i18n$t("Report"),
                                               tabName = "report",
                                               icon = icon("print"))
      )


      slick <- Slick_Object()

      if (!is.null(slick)) {
        chk <- Check(slick)
        slick <<- slick
        LL <- ll
        map <- data.frame(plot=c("Boxplot", "Kobe", "Quilt",
                                 "Spider", "Timeseries", "Tradeoff"),
                          ind=c(4,5,6,7,3,8))
        for (p in 1:nrow(map)) {
          if(chk@empty[[ map$plot[p]]]) {
            ll[[2]][[map$ind[p]]] <- NA
          }
        }
        ll[[2]] <- Filter(function(a) any(!is.na(a)), ll[[2]])
      }


      ll
    })

    selection <- reactive({
      if (Load_Slick_File$loaded >=1) {

        return(2)
      }

      1
    })


    output$sidebar <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(id='sidebarmenu', .list=menu_list()[[selection()]])
    })
  })
}

## To be copied in the UI
# mod_Sidebar_ui("Sidebar_1")

## To be copied in the server
# mod_Sidebar_server("Sidebar_1")

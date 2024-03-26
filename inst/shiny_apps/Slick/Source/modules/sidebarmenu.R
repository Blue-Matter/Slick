mod_sidebar_main_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::sidebarMenuOutput(ns('sidebar'))
  )
}

mod_sidebar_main_server <- function(id, Object, i18n){
  ns <- NS(id)

  moduleServer( id, function(input, output, session){

    menu_list <- reactive({
      i18n <- i18n()
      ll <- list(menuItem(i18n$t('Home'), tabName='home', icon=icon('home')))

      if (Object$Loaded) {
        ll <- list(menuItem(i18n$t('Home'), tabName='home', icon=icon('home')),
                   menuItem(i18n$t("Overview"), tabName = "metadata", icon = icon("info-circle")),
                   menuItem("Deterministic", icon = icon("chart-bar", verify_fa=FALSE), startExpanded = TRUE,
                            menuSubItem("Spider", tabName = "spider",
                                        icon = shiny::icon("angle-double-right",verify_fa = FALSE))
                   )
        )

      }
      ll
    })

    output$sidebar <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(id='NonTech', .list=menu_list())
    })


  })
}
#
#
# sidebarMenu(id='NonTech',
#             menuItem("Home", tabName = "home", icon = icon("house")),
#             menuItem("Overview", tabName = "metadata", icon = icon("info-circle")),
#
#             # Deterministic
#             menuItem("Deterministic", icon = icon("chart-bar", verify_fa=FALSE), startExpanded = TRUE,
#                      menuSubItem("Spider", tabName = "spider",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Spider OM", tabName = "spiderOM",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Zigzag", tabName = "zigzag",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Rail", tabName = "rail",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE))
#             ),
#             # Stochastic
#             menuItem("Stochastic", icon = icon("chart-scatter", verify_fa=FALSE), startExpanded = TRUE,
#                      menuSubItem("Boxplot", tabName = "boxplot",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Boxplot OM", tabName = "boxplotOM",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Violin", tabName = "violin",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE))
#
#             ),
#             # Projected
#             menuItem("Projection", icon = icon("chart-line", verify_fa=FALSE), startExpanded = TRUE,
#                      menuSubItem("Kobe", tabName = "kobe",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Kobe Time", tabName = "kobetime",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Slope", tabName = "slope",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE))
#             ),
#             # State Variables
#             menuItem("Time Series", icon = icon("layer-group"), startExpanded = TRUE,
#                      menuSubItem("Line", tabName = "line",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Line OM", tabName = "lineOM",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
#                      menuSubItem("Line OM Sim", tabName = "lineOMSim",
#                                  icon = shiny::icon("angle-double-right",verify_fa = FALSE))
#             )
#
# )

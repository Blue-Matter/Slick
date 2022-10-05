library(shinydashboard)
library(shinydashboardPlus)
library(shiny.i18n)
library(fresh)
library(shinyWidgets)
library(shinyBS)


mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

header <-  dashboardHeader(title = tagList(shiny.i18n::usei18n(i18n),
                                           i18n$t("Slick Decision Analysis"))
                           )


# todo
# make bigger text in side-bar
# style - match existing colors etc

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "homepage", icon = icon("home")),
    menuItem("Load Slick Object", tabName = "splash", icon = icon("upload")),

    menuItem("Charts", icon = icon("bar-chart-o"), startExpanded = TRUE,
             menuSubItem("Spider", tabName = "spider"),
             menuSubItem("Zigzag", tabName = "zigzag")
             ),

    menuItem("Change Language", icon = icon("language"), tabName = "language",
    selectInput('selected_language',
                i18n$t("Select language"),
                choices = languages,
                selected = i18n$get_key_translation()))
  )
)




body <- dashboardBody(
  tags$head(
    includeScript(path = "www/js/js4checkbox.js"),
    includeScript(path = "www/js/index.js")
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            h2(i18n$t('Hello')),
            p(i18n$t('test'))
    ),
    tabItem(tabName = "splash",
                SplashUI('splash')
    ),
    tabItem(tabName = "spider",
            SpiderUI('spider')
    )
  )
)

#todo
# add title to rhs filter
dashboardControlbar2 <- function (..., id = NULL, disable = FALSE, width = 230, collapsed = TRUE,
                                  overlay = TRUE, skin = "dark", .list = NULL) {
  items <- c(list(...), .list)
  if (is.null(id))
    id <- "controlbarId"
  controlbarTag <- shiny::tagList(shiny::tags$aside(id = id,
                                                    `data-collapsed` = if (collapsed)
                                                      "true"
                                                    else "false", `data-overlay` = if (overlay)
                                                      "true"
                                                    else "false", `data-show` = if (disable)
                                                      "false"
                                                    else "true", class = paste0("control-sidebar control-sidebar-",
                                                                                skin), style = paste0("width: ", width, "px;"),
                                                    items), shiny::tags$div(class = "control-sidebar-bg"))
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(".control-sidebar-bg,\n               .control-sidebar {\n                  top: 0;\n                  right: ",
                                                                                        -width, "px;\n                  width: ", width, "px;\n                  -webkit-transition: right 0.3s ease-in-out;\n                  -o-transition: right 0.3s ease-in-out;\n                  transition: right 0.3s ease-in-out;\n               }\n              /* .control-sidebar-open .control-sidebar, .control-sidebar-open .control-sidebar-bg {\n                right: ",
                                                                                        -width, "px;\n              } */\n              @media (min-width:768px) {\n                .control-sidebar-open .content-wrapper,\n                .control-sidebar-open .main-footer, \n                .control-sidebar-open .right-side {\n                  margin-right: ",
                                                                                        width, "px;\n                }\n              }\n              "))))),
                 controlbarTag)
}

controlbar <- dashboardControlbar2(overlay = FALSE,
                                  FiltersUI('filters')

)




dashboardPage(
  header,
  sidebar,
  body,
  controlbar=controlbar,
  title='Slick Decision Analysis',
  dashboardFooter(left = "Left content", right = "Right content"),
  freshTheme = mytheme
)

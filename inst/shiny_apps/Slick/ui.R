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

header <-  dashboardHeader2(title = tagList(shiny.i18n::usei18n(i18n),
                                           i18n$t("Slick Decision Analysis")),
                           controlbarIcon=shiny::icon('filter')
                           )

#todo
# add title to rhs filter

controlbar <- dashboardControlbar(overlay = FALSE,
                                  width=450,
                                  skin='light',
                                  FiltersUI('filters')

)


# todo
# make bigger text in side-bar
# style - match existing colors etc

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "homepage", icon = icon("house")),
    menuItem("Load Slick Object", tabName = "splash", icon = icon("upload")),

    menuItem("Charts", icon = icon("chart-simple"), startExpanded = TRUE,
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
    includeScript(path = "www/js/index.js"),
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),
    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$style(HTML("#SessionID{font-size:12px;}")),
    tags$style(HTML("/* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */
        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');
        /* Font of SLICK title */
      ")),
    tags$script(
    'var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    ')
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






dashboardPage(
  header,
  sidebar,
  body,
  controlbar=controlbar,
  title='Slick Decision Analysis',
  dashboardFooter(left = "Left content", right = "Right content")
)

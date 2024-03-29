# -- theme ----
Slick_theme <- function() {
  fresh::create_theme(
    fresh::adminlte_global(
      content_bg = "#FFFFFF"
    ),
    fresh::adminlte_sidebar(
      dark_bg = '#37638a',
      dark_hover_bg ='#143570'

    ),

    fresh::adminlte_color(
      light_blue = "#37638a"
    ),
    fresh::adminlte_vars(
      'sidebar-width'='300px'

    )
  )
}



# -- header ----
header <- function() {
  dashboardHeader2(title = tagList(shiny.i18n::usei18n(set_translator()),
                                                      # tags$a(href='https://harveststrategies.org/',
                                                      #        target="_blank",
                                                      #        tags$img(src='www/img/logo.png',
                                                      #                 height = '50', width ='300')
                                                      # ),
  ),
  leftUi = tagList(
    shinyWidgets::dropdownButton(
      label = "Switch Language",
      icon = icon("language"),
      status = "primary",
      circle = FALSE,
      uiOutput("language")
    ),
    shinyWidgets::dropdownButton(
      width=900,
      label = "Resources",
      icon = icon("book", verify_fa = FALSE),
      status = "primary",
      circle = FALSE,
      mod_Resources_ui('resources')

    ),
    shinyWidgets::dropdownButton(
      width=500,
      label = "About",
      icon = icon("info"),
      status = "primary",
      circle = FALSE,
      mod_About_ui("about")
    ),
    conditionalPanel('output.Loaded>0',
                     shinyWidgets::dropdownButton(
                       width=700,
                       label = "Management Procedures",
                       status = "primary",
                       circle = FALSE,
                       mod_MP_Info_ui("MPheader")
                     )
    ),
    conditionalPanel('output.Loaded>0',
                     shinyWidgets::dropdownButton(
                       width=700,
                       label = "Operating Models",
                       status = "primary",
                       circle = FALSE,
                       mod_OM_Info_ui("OMheader")
                     )
    ),
    conditionalPanel('output.Loaded>0',
                     shinyWidgets::dropdownButton(
                       width=700,
                       label = "Performance Metrics",
                       status = "primary",
                       circle = FALSE,
                       mod_PM_Info_ui("PMheader")
                     )
    )
  ),
  controlbarIcon=shiny::icon('filter')
  )
}


# -- rhs controlbar ----
controlbar <- function() {
  shinydashboardPlus::dashboardControlbar(disable = TRUE)
  # shinydashboardPlus::dashboardControlbar(overlay = FALSE,
  #                      width=450,
  #                      skin='light',
  #                      collapsed = TRUE,
  #                      mod_Filters_ui('filters')
  #
  # )
}

# -- lhs sidebar ----
sidebar <- function() {
  shinydashboardPlus::dashboardSidebar(
    width = "0px",
    collapsed = TRUE,
    disable = TRUE,
    mod_Sidebar_ui("sidebar")
  )
}


# -- body ----
body <- function() {
  shinydashboard::dashboardBody(

    tags$head(
      # includeScript(path = "/js/js4checkbox.js"),
      # includeScript(path = "/js/index.js"),
      tags$link(rel='stylesheet', type='text/css', href='styles.css'),
      tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
      tags$link(rel="shortcut icon", href="favicon.ico"),

      tags$style(HTML("#SessionID{font-size:12px;}")),
      tags$style(HTML("/* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */
        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');
        /* Font of SLICK title */
      "))

    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "hometab",
                              mod_Home_ui("home")
      ),
      shinydashboard::tabItem(tabName = "metadatatab",
                              mod_Metadata_ui("metadata")

      ),
      shinydashboard::tabItem(tabName = "quilt",
                              mod_Quilt_ui("Quilt")

      )

      # # tabItem(tabName = "load",
      # #         LoadUI('load')
      # # ),
      #
      # # Deterministic
      # shinydashboard::tabItem(tabName = "spider",
      #         SpiderUI('spider')
      # ),
      # shinydashboard::tabItem(tabName = "spiderOM",
      #         value='det',
      #         Spider_OMUI('spiderOM')
      # ),
      # shinydashboard::tabItem(tabName = "zigzag",
      #         ZigzagUI('zigzag')
      # ),
      # shinydashboard::tabItem(tabName = "rail",
      #         RailUI('rail2')
      # ),
      # # Stochastic
      # shinydashboard::tabItem(tabName = "boxplot",
      #         value='stoch',
      #         BoxplotUI('boxplot')
      # ),
      # shinydashboard::tabItem(tabName = "boxplotOM",
      #         value='stoch',
      #         Boxplot_OMUI('boxplotOM')
      # ),
      # shinydashboard::tabItem(tabName = "violin",
      #         ViolinUI('violin')
      # ),
      # # Projected
      # shinydashboard::tabItem(tabName = "kobe",
      #         value='proj',
      #         KobeUI('kobe')
      # ),
      # shinydashboard::tabItem(tabName = "kobetime",
      #         value='proj',
      #         KobeTimeUI('kobetime')
      # ),
      # shinydashboard::tabItem(tabName = "slope",
      #         value='proj',
      #         SlopeUI('slope')
      # ),
      # # State
      # shinydashboard::tabItem(tabName = "line",
      #         value='state',
      #         LineUI('line')
      # ),
      # shinydashboard::tabItem(tabName = "lineOM",
      #         value='state',
      #         Line_OMUI('lineOM')
      # ),
      # shinydashboard::tabItem(tabName = "lineOMSim",
      #         value='state',
      #         Line_OMSimUI('lineOMSim')
      # )
    )
  )
}



#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    shinydashboardPlus::dashboardPage(
      header=header(),
      sidebar=sidebar(),
      body=body(),
      controlbar=controlbar(),
      title='Slick',
      shinydashboardPlus::dashboardFooter(left = paste0("Slick version:", packageVersion('Slick')),
                      right = tags$a(href='https://harveststrategies.org/',
                                     target="_blank", paste0("harveststrategies.org ", format(Sys.Date(), "%Y"))))
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Slick"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    fresh::use_theme(Slick_theme()),
    waiter::useWaiter(),
    shinyjs::useShinyjs(),
    waiter::waiterPreloader(waiter::spin_fading_circles())

  )
}

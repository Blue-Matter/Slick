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
  shinydashboardPlus::dashboardHeader(title = tagList(shiny.i18n::usei18n(set_translator())),
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
                       inputId ='mpdropdown',
                       width=700,
                       label = "MPs",
                       status = "primary",
                       circle = FALSE,
                       mod_MP_Info_ui("MPheader")
                     )
    ),
    conditionalPanel('output.Loaded>0',
                     shinyWidgets::dropdownButton(
                       inputId ='omdropdown',
                       width=700,
                       label = "OMs",
                       status = "primary",
                       circle = FALSE,
                       mod_OM_Info_ui("OMheader")
                     )
    ),
    conditionalPanel('output.Loaded>0',
                     shinyWidgets::dropdownButton(
                       inputId ='pmdropdown',
                       width=700,
                       label = "PIs",
                       status = "primary",
                       circle = FALSE,
                       mod_PM_Info_ui("PMheader")
                     )
    )
  ),
  controlbarIcon=icon('fa-xl fa-filter', class='fa-regular')
  )
}


# -- rhs controlbar ----
controlbar <- function() {

  shinydashboardPlus::dashboardControlbar(overlay = TRUE,
                                          id='controlbar',
                                          width=450,
                                          skin='light',
                                          collapsed = TRUE,
                                          mod_Global_Filters_ui('filters')

  )
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
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "hometab",
                              mod_Home_ui("home")
      ),
      shinydashboard::tabItem(tabName = "metadatatab",
                              mod_Metadata_ui("metadata")

      ),
      shinydashboard::tabItem(tabName = "boxplot",
                              mod_Boxplot_ui("Boxplot")
      ),
      shinydashboard::tabItem(tabName = "kobe",
                              mod_Kobe_ui("Kobe")
      ),
      shinydashboard::tabItem(tabName = "quilt",
                              mod_Quilt_ui("Quilt")
      ),
      shinydashboard::tabItem(tabName = "spider",
                              mod_Spider_ui("Spider")
      ),
      shinydashboard::tabItem(tabName = "timeseries",
                              mod_Timeseries_ui("Timeseries_1")
      ),
      shinydashboard::tabItem(tabName = "tradeoff",
                              mod_Tradeoff_ui("Tradeoff")
      ),
      shinydashboard::tabItem(tabName = "report",
                              mod_Report_Page_ui("Report_Page_1")
      )
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
      footer=shinydashboardPlus::dashboardFooter(left = paste0("Slick version: ", slickVersion()),
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
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Slick"
    ),
    # tags$link(href="all.css", rel="stylesheet"), # font-awesome
    tags$style(
      type = 'text/css',
      '.modal-dialog { width: fit-content !important; }'
    ),
    tags$script(src="https://kit.fontawesome.com/579eb08a76.js"),


    fresh::use_theme(Slick_theme()),
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    waiter::useAttendant(),
    waiter::use_waitress(),
    waiter::waiterPreloader(waiter::spin_fading_circles()),



  )
}


# tags$head(
#   # includeScript(path = "/js/js4checkbox.js"),
#   # includeScript(path = "/js/index.js"),
#   tags$link(rel='stylesheet', type='text/css', href='styles.css'),
#
#   tags$link(rel="shortcut icon", href="favicon.ico"),
#   tags$style(HTML("#SessionID{font-size:12px;}")),
#   tags$style(HTML("/* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */
#         @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');
#         /* Font of SLICK title */
#       "))
#
# ),

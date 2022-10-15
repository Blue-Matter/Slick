#todo
# - add help menu and links to userguides


# -- theme ----
Slick_theme <- create_theme(
  adminlte_global(
    content_bg = "#FFFFFF"
  ),
  adminlte_sidebar(
    dark_bg = '#37638a',
    dark_hover_bg ='#143570'
  ),
  adminlte_color(
    light_blue = "#086A87"
  ),
  adminlte_vars(
    'sidebar-width'='300px'

  )
)

# -- header ----
header <-  dashboardHeader2(title = tagList(shiny.i18n::usei18n(translator),
                                            "Slick Decision Analysis"),
                            leftUi = tagList(
                              dropdownButton(
                                label = "Switch Language",
                                icon = icon("language"),
                                status = "primary",
                                circle = FALSE,
                                uiOutput("language")
                              ),
                              dropdownButton(
                                width=1200,
                                label = "Resources",
                                icon = icon("books"),
                                status = "primary",
                                circle = FALSE,
                                ResourcesUI('resources')

                              ),
                              dropdownButton(
                                width=500,
                                label = "About",
                                icon = icon("info"),
                                status = "primary",
                                circle = FALSE,
                                uiOutput("about")
                              )
                            ),
                           controlbarIcon=shiny::icon('filter')
                           )



# -- rhs controlbar ----
controlbar <- dashboardControlbar(overlay = FALSE,
                                  width=450,
                                  skin='light',

                                  FiltersUI('filters', i18n=i18n)

)

# -- lhs sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("house")),
    menuItem("Load", tabName = "load", icon = icon("upload")),

    menuItem("Summary", icon = icon("chart-line"), startExpanded = TRUE,
             menuSubItem("Spider", tabName = "spider"),
             menuSubItem("Zigzag", tabName = "zigzag")
             )
  )
)


# -- body ----
body <- dashboardBody(
  use_theme(Slick_theme),
  useWaiter(),
  waiterPreloader(spin_fading_circles()),

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
    '),
    tags$script("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")

  ),
  tabItems(
    tabItem(tabName = "home",
            HomeUI('home')

    ),
    tabItem(tabName = "load",
            LoadUI('load')

    ),
    tabItem(tabName = "spider",
            SpiderUI('spider')
    )
  )
)


# -- page ----
dashboardPage(
  header=header,
  sidebar=sidebar,
  body=body,
  controlbar=controlbar,
  title='Slick Decision Analysis',
  dashboardFooter(left = "Left content", right = "Right content")
)

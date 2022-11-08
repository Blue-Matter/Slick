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
                                width=900,
                                label = "Resources",
                                icon = icon("books", verify_fa = FALSE),
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
                                  collapsed = FALSE,

                                  FiltersUI('filters', i18n=i18n)

)

# -- lhs sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(id='NonTech',
    menuItem("Home", tabName = "home", icon = icon("house")),
    menuItem("Load", tabName = "load", icon = icon("upload")),

    # Deterministic
    menuItem("Deterministic", icon = icon("chart-bar", verify_fa=FALSE), startExpanded = TRUE,
             menuSubItem("Spider", tabName = "spider",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Spider OM", tabName = "spiderOM",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Zigzag", tabName = "zigzag",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Rail", tabName = "rail",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE))
             ),
    # Stochastic
    menuItem("Stochastic", icon = icon("chart-scatter", verify_fa=FALSE), startExpanded = TRUE,
             menuSubItem("Boxplot", tabName = "boxplot",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Boxplot OM", tabName = "boxplotOM",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Violin", tabName = "violin",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE))

    ),
    # Projected
    menuItem("Projected ", icon = icon("chart-line", verify_fa=FALSE), startExpanded = TRUE,
             menuSubItem("Kobe", tabName = "kobe",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Kobe Time", tabName = "kobetime",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Slope", tabName = "slope",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE))
    ),
    # State Variables
    menuItem("State Variables", icon = icon("layer-group"), startExpanded = TRUE,
             menuSubItem("Line", tabName = "line",
                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)),
             menuSubItem("Line OM", tabName = "lineOM",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE))
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

    # Deterministic
    tabItem(tabName = "spider",
            SpiderUI('spider')
    ),
    tabItem(tabName = "spiderOM",
            value='det',
            Spider_OMUI('spiderOM')
    ),
    tabItem(tabName = "zigzag",
            ZigzagUI('zigzag')
    ),
    tabItem(tabName = "rail",
            RailUI('rail')
    ),
    # Stochastic
    tabItem(tabName = "boxplot",
            value='stoch',
            BoxplotUI('boxplot')
    ),
    tabItem(tabName = "boxplotOM",
            value='stoch',
            Boxplot_OMUI('boxplotOM')
    ),
    tabItem(tabName = "violin",
            ViolinUI('violin')
    ),
    # Projected
    tabItem(tabName = "kobe",
            value='proj',
            KobeUI('kobe')
    ),
    tabItem(tabName = "kobetime",
            value='proj',
            KobeTimeUI('kobetime')
    ),
    tabItem(tabName = "slope",
            value='proj',
            SlopeUI('slope')
    ),
    # State
    tabItem(tabName = "line",
            value='state',
            LineUI('line')
    ),
    tabItem(tabName = "lineOM",
            value='state',
            Line_OMUI('lineOM')
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
  dashboardFooter(left = paste0("Slick version:", packageVersion('Slick')),
                  right = paste0("The Ocean Foundation ", format(Sys.Date(), "%Y")))
)

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

                                  FiltersUI('filters', i18n=i18n)

)

# -- lhs sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(id='NonTech',
    menuItem("Home", tabName = "home", icon = icon("house")),
    menuItem("Load", tabName = "load", icon = icon("upload")),

    menuItem("Summary", icon = icon("chart-line"), startExpanded = FALSE,
             convertMenuItem(menuSubItem("Spider", tabName = "spider",
                                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'det'),
             convertMenuItem(menuSubItem("Zigzag", tabName = "zigzag",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'det'),
             convertMenuItem(menuSubItem("Rail", tabName = "rail",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'det'),
             convertMenuItem(menuSubItem("Violin", tabName = "violin",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), ''),
             convertMenuItem(menuSubItem("Boxplot", tabName = "boxplot",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'stoch'),
             convertMenuItem(menuSubItem("Kobe", tabName = "kobe",
                                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'proj')
             ),
    menuItem("Trade-Off", icon = icon("code-compare"), startExpanded = FALSE,
             convertMenuItem(menuSubItem("Slope", tabName = "slope",
                                         icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'proj')
    ),
    menuItem("Over Time", icon = icon("timeline"), startExpanded = FALSE,
             convertMenuItem(menuSubItem("Kobe Time", tabName = "kobetime",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'proj'),
             convertMenuItem(menuSubItem("Line", tabName = "line",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'state'),
             convertMenuItem(menuSubItem("Worm", tabName = "worm",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), '')
    ),
    menuItem("By OM", icon = icon("layer-group"), startExpanded = FALSE,
             convertMenuItem(menuSubItem("Boxplot OM", tabName = "boxplotOM",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'stoch'),
             convertMenuItem(menuSubItem("Spider OM", tabName = "spiderOM",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)),'det'),
             convertMenuItem(menuSubItem("Line OM", tabName = "lineOM",
                             icon = shiny::icon("angle-double-right",verify_fa = FALSE)), 'state')

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
    ),
    tabItem(tabName = "zigzag",
            ZigzagUI('zigzag')
    ),
    tabItem(tabName = "rail",
            RailUI('rail')
    ),
    # tabItem(tabName = "violin",
    #         ViolinUI('violin')
    # ),
    tabItem(tabName = "boxplot",
            value='stoch',
            BoxplotUI('boxplot')
    ),
    tabItem(tabName = "kobe",
            value='proj',
            KobeUI('kobe')
    ),
    tabItem(tabName = "slope",
            value='proj',
            SlopeUI('slope')
    ),
    tabItem(tabName = "kobetime",
            value='proj',
            KobeTimeUI('kobetime')
    ),
    tabItem(tabName = "line",
            value='state',
            LineUI('line')
    ),
    # tabItem(tabName = "worm",
    #         value='proj',
    #         LineUI('worm')
    # ),
    tabItem(tabName = "boxplotOM",
            value='stoch',
            Boxplot_OMUI('boxplotOM')
    ),
    tabItem(tabName = "spiderOM",
            value='det',
            Spider_OMUI('spiderOM')
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
  dashboardFooter(left = "Left content", right = "Right content")
)

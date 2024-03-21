
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
    light_blue = "#37638a"
  ),
  adminlte_vars(
    'sidebar-width'='300px'

  )
)

# -- header ----
header <- dashboardHeader2(title = tagList(shiny.i18n::usei18n(translator),
                                           tags$a(href='https://harveststrategies.org/',
                                                  target="_blank",
                                                  tags$img(src='img/logo.png',
                                                           height = '50', width ='300')
                                                  ),
                                           ),
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
                              ),
                              conditionalPanel('output.Loaded>0',
                                               dropdownButton(
                                                 width=700,
                                                 label = "MP Details",
                                                 status = "primary",
                                                 circle = FALSE,
                                                 uiOutput("mp_details")
                                               )
                              ),
                              conditionalPanel('output.Loaded>0',
                                               dropdownButton(
                                                 width=700,
                                                 label = "OM Details",
                                                 status = "primary",
                                                 circle = FALSE,
                                                 uiOutput("om_details")
                                               )
                              ),
                              conditionalPanel('output.Loaded>0',
                                               dropdownButton(
                                                 width=700,
                                                 label = "PM Details",
                                                 status = "primary",
                                                 circle = FALSE,
                                                 uiOutput("pm_details")
                                               )
                              )
                            ),
                           controlbarIcon=shiny::icon('filter')
                           )

# -- rhs controlbar ----
controlbar <- dashboardControlbar(overlay = FALSE,
                                  width=450,
                                  skin='light',
                                  collapsed = TRUE,
                                  FiltersUI('filters', i18n=i18n)

)

# -- lhs sidebar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  collapsed = TRUE,
  disable = TRUE,
  mod_sidebar_main_ui("sidebar_main_1")

)


# -- body ----
body <- dashboardBody(
  use_theme(Slick_theme),
  useWaiter(),
  shinyjs::useShinyjs(),
  waiterPreloader(spin_fading_circles()),

  tags$head(
    includeScript(path = "www/js/js4checkbox.js"),
    includeScript(path = "www/js/index.js"),
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),
    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$link(rel="shortcut icon", href="favicon.ico"),

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
    tabItem(tabName = "metadata",
            metadataUI('metadata')

    ),
    # tabItem(tabName = "load",
    #         LoadUI('load')
    # ),

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
            RailUI('rail2')
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
    ),
    tabItem(tabName = "lineOMSim",
            value='state',
            Line_OMSimUI('lineOMSim')
    )
  )
)


# -- page ----
shinydashboardPlus::dashboardPage(
  header=header,
  sidebar=sidebar,
  body=body,
  controlbar=controlbar,
  title='Slick',
  dashboardFooter(left = paste0("Slick version:", packageVersion('Slick')),
                  right = tags$a(href='https://harveststrategies.org/',
                                 target="_blank", paste0("harveststrategies.org ", format(Sys.Date(), "%Y"))))
)

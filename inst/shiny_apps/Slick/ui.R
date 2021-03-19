library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinydashboard)

fluidPage(

  includeScript(path = "www/js/js4checkbox.js"),
  includeScript(path = "www/js/index.js"),

  tags$head(
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),

    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$style(HTML("
                    #SessionID{font-size:12px;}
                    ")),
    tags$style(HTML("
        /* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */

        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');

        /* Font of SLICK title */

      ")),
    tags$script('
                                var dimension = [0, 0];
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

  # === HEADER ==============================================================================================================================================================
  column(12,
         column(6,h2("Slick decision analysis"),style="height:65px")
         #column(5,style="height:65px",
          #      h2("decision analysis",style="padding-top:22px;padding-left:4px")

  ),

  # === MAIN WINDOW =========================================================================================================================================================
  column(12,

    # --- General results -----------------------------------------------------------------
    # conditionalPanel('input.Mode=="General"',

      column(10, # General tab panel
        verticalTabsetPanel(id = "NonTech",selected=1,

                            verticalTabPanel(value="splash",
                                             h5(strong("Home")),
                                             SplashUI('splash'),
                                             box_height='50px',
                                             id='splash'),
                            verticalTabPanel(value="det",
                                             h5("Spider"),
                                             SpiderUI('spider'),
                                             box_height='50px'),

                            verticalTabPanel(value="det",
                                             h5("Zigzag"),
                                             ZigzagUI('zigzag'),
                                             box_height='50px'),

                            verticalTabPanel(value='det',
                                             h5("Rail"),
                                             RailUI('rail'),
                                             box_height='50px'),

                            verticalTabPanel(value='proj',
                                             h5("Kobe"),
                                             KobeUI('kobe'),
                                             box_height='50px'),

                            verticalTabPanel(value='proj',
                                             h5("Kobe Time"),
                                             KobeTimeUI('kobetime'),
                                             box_height='50px'),

                            verticalTabPanel(value='state',
                                             h5("Line"),
                                             LineUI('line'),
                                             box_height='50px'),

                            verticalTabPanel(value='proj',
                                             h5("Slope"),
                                             SlopeUI('slope'),
                                             box_height='50px'),

                            verticalTabPanel(value='stoch',
                                             h5("Boxplot"),
                                             BoxplotUI('boxplot'),
                                             box_height='50px'),

                            verticalTabPanel(value='stoch',
                                             h5("Boxplot OM"),
                                             Boxplot_OMUI('boxplotOM'),
                                             box_height='50px'),

                            verticalTabPanel(value='det',
                                             h5("Spider OM"),
                                             Spider_OMUI('spiderOM'),
                                             box_height='50px'),

                            verticalTabPanel(value='state',
                                             h5("Line OM"),
                                             Line_OMUI('lineOM'),
                                             box_height='50px'),
                            verticalTabPanel(value='resources',
                                             h5(strong("Resources")),
                                             ResourcesUI('resources'),
                                             box_height='50px'),

          contentWidth=11
         # box_height='150px'

          ) # end of tabsetpanel

       ), # end of main window for general

      # filtering for General results
    FiltersUI('filters')


    # ), # end of conditional panel general
  ), # end of main window


  column(12,  br(),br(), style="height:40px;  text-align: center;",textOutput("SessionID")),

  column(12,  br(),style="height:40px; text-align: center", h6("copyright (c) The Ocean Foundation, 2021"))
        #h5("Bottom of app (Version etc)"),
         #verbatimTextOutput("Log2",placeholder=T)

         # verbatimTextOutput("Temp",placeholder=T)
         #)

) # end of fluid page

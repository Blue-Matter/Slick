#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Home_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns('main'))
  )
}

#' Home Server Functions
#'
#' @noRd
mod_Home_server <- function(id, i18n, Load_Slick_File, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # mod_links_server(id, Object, i18n)
    output$main <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(title=h3(strong(i18n$t('Welcome to Slick'))),
            width=12,
            solidHeader=TRUE,
            status = "primary",
            uiOutput(ns('welcome')),
            uiOutput(ns('howtouse')),
            uiOutput(ns('load'))
        )
        # mod_links_ui(ns(id))
      )
    })

    output$welcome <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=5,
                                solidHeader=FALSE,
                                status = "primary",
                                title=strong(i18n$t('About Slick')),
                                p(i18n$t(
                                  'Slick is a decision analysis tool that presents the outcomes of candidate harvest strategies across various states of nature, accounting for uncertainty. Slick is interactive and allows users to filter results live in order to explore robustness and performance.'
                                )),
                                p(i18n$t(
                                  'While Slick can be applied to any decision analysis context it was specifically designed to investigate the performance of harvest strategies tested by management strategy evaluation (MSE). Slick can be used for any MSE process, but the current version includes a demonstration example, as well as preliminary results from the MSEs for Western Atlantic skipjack tuna and North Atlantic swordfish.'
                                ))
        ),
      )
    })

    output$howtouse <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=4,
                                solidHeader=FALSE,
                                status = "primary",
                                title=strong(i18n$t('How to use Slick')),
                                tags$ol(
                                  tags$li(
                                    strong(i18n$t('Load a Slick Data File:')),
                                    p(i18n$t('To use Slick, you need to either choose from one of the examples, or upload a Slick Data file. Consult the'), a(href='https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html', "Developer's Guide",  target="_blank"), i18n$t('for information on creating a Slick Data file.'))
                                  ),
                                  tags$li(
                                    strong(i18n$t('Explore the Results:')),
                                    p(i18n$t("After a Slick Data File is loaded, the App will navigate to an Overview page which provides a summary of the information contained in the Slick Data File, and links to Slick's data visualization pages.")
                                    )

                                  )
                                )

        )
      )
    })

    output$load <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=3,
                                solidHeader=FALSE,
                                status = "primary",
                                title=strong(i18n$t('Load Slick Data')),
                                h4(i18n$t('Load your MSE Results')),
                                fileInput(ns("load"), accept=c("slick",".slick"),
                                          label = i18n$t("From file (.slick)"),
                                          buttonLabel=list(icon("folder",verify_fa = FALSE))
                                ),
                                h4(i18n$t('Load an Example')),
                                selectInput(ns('example_input'),
                                            label=i18n$t('Example'),
                                            choices=case_study_df$Example,
                                            selected=NULL
                                ),
                                actionButton(ns("example_upload"), i18n$t("Load"),
                                             icon("upload", verify_fa = FALSE)),
                                downloadButton(ns("example_download"), i18n$t("Download"),
                                               icon("cloud-download", verify_fa = FALSE))
        )
      )
    })

    observeEvent(input$load, ignoreInit = TRUE, {
      Load_Slick_File$file <- input$load
      Load_Slick_File$loaded <- Load_Slick_File$loaded +1
      shinyjs::delay(30,
                     shinyjs::runjs("$('a[data-value=\"metadatatab\"]').tab('show');")
      )
    })

    observeEvent(input$example_upload, ignoreInit = TRUE, {
      Load_Slick_File$file <- input$example_input
      Load_Slick_File$loaded <- Load_Slick_File$loaded +1
      shinyjs::delay(30,
                     shinyjs::runjs("$('a[data-value=\"metadatatab\"]').tab('show');")
      )
    })

    observeEvent(Load_Slick_File$loaded, ignoreInit = TRUE, {
      if (Load_Slick_File$loaded >= 1) {
        # Object$Ready <- FALSE
        # Object$Filt <- FALSE
        if (inherits(Load_Slick_File$file, 'character')) {
          slick <- get(case_study_df$Object[match(Load_Slick_File$file, case_study_df$Example)])
          if(inherits(slick, 'Slick'))
            slick <- Slick2SlickData(slick)
          Slick_Object(slick)
        }
        if (inherits(Load_Slick_File$file, 'data.frame')) {
          slick <- readRDS(Load_Slick_File$file$datapath)
          if(inherits(slick, 'Slick'))
            slick <- Slick2SlickData(slick)
          Slick_Object(slick)
        }
      }
    })

  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")

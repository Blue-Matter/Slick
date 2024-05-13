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
mod_Home_server <- function(id, i18n, Load_Slick_File, Slick_Object, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$main <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(title=h3(strong(i18n$t('Welcome to Slick'))),
            width=12,
            solidHeader=TRUE,
            status = "primary",
            uiOutput(ns('welcome')),
            uiOutput(ns('howtouse')),
            uiOutput(ns('load')),
            id=ns('mainbox')
        )
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
                                # uiOutput(ns('overviewlink')),
                                h4(i18n$t('Load your MSE Results')),
                                fileInput(ns("load"), accept=c("slick",".slick"),
                                          label = i18n$t("From file (.slick)"),
                                          buttonLabel=list(icon("folder",verify_fa = FALSE))
                                ),
                                h4(i18n$t('Load an Example')),
                                selectInput(ns('example_input'),
                                            label=i18n$t('Example'),
                                            choices=Slick::case_study_df$Example,
                                            selected=NULL
                                ),
                                actionButton(ns("example_upload"), i18n$t("Load"),
                                             icon("upload", verify_fa = FALSE)),
                                downloadButton(ns("example_download"), i18n$t("Download"),
                                               icon("cloud-download", verify_fa = FALSE))
        )
      )
    })

    output$example_download <- downloadHandler(
      filename = function() {
        Name <- input$example_input
        paste0(Name, ".slick", sep="")
      },
      content = function(file) {
        Name <- input$example_input
        File <- Slick::case_study_df$Object[match(Name, Slick::case_study_df$Example)]
        obj <- readRDS(app_sys(paste0(File, '.rda')))
        obj <- Update(obj)
        saveRDS(obj, file)
      }
    )

    observeEvent(input$load, ignoreInit = TRUE, {
      Load_Slick_File$file <- input$load
      Load_Slick_File$loaded <- Load_Slick_File$loaded + 1
    })

    observeEvent(input$example_upload, ignoreInit = TRUE, {
      Load_Slick_File$file <- input$example_input
      Load_Slick_File$loaded <- Load_Slick_File$loaded +1
    })

    observeEvent(input$overview, {
      shinyjs::runjs("$('a[data-value=\"metadatatab\"]').tab('show');")
    })


    loaded_slick <- reactiveVal()

    observeEvent(Load_Slick_File$loaded, ignoreInit = TRUE, {

      if (Load_Slick_File$loaded >= 1) {
        if (inherits(Load_Slick_File$file, 'character')) {
          File <- Slick::case_study_df$Object[match(Load_Slick_File$file, Slick::case_study_df$Example)]
          slick <- readRDS(app_sys(paste0(File, '.rda')))
          check_slick_file(slick)
        }
        if (inherits(Load_Slick_File$file, 'data.frame')) {
          slick <- try(readRDS(Load_Slick_File$file$datapath))
          if (inherits(slick, 'try-error')) {
            shinyalert::shinyalert('Incorrect File Type',
                                   'Could not import file. Is it a Slick object created with `saveRDS?`',
                                   type='error')
          } else {
            check_slick_file(slick)
          }
        }
      }
    })

    check_slick_file <- function(slick) {
      if (!inherits(slick, 'Slick')) {
        shinyalert::shinyalert('Incorrect File Type',
                               'The loaded file is not a Slick object',
                               type='error')
        return(NULL)
      }

      # update
      if (!isS4(slick))
        slick <- Update(slick)

      # check
      slick <- try(Check(slick))

      if (inherits(slick, 'try-error')) {
        shinyalert::shinyalert('Invalid Slick object',
                               'Use `Check(`slick_object`)` to see the errors',
                               type='error')
      }

      # set MP colors
      slick <- slick
      #
      Slick_Object(slick)


      # jump to metadata tab
      shinyjs::delay(10,
                     shinyjs::runjs("$('a[data-value=\"metadatatab\"]').tab('show');")
      )

    }


  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")

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
                                  'Slick is a decision analysis tool that presents the outcomes of candidate management procedures (MPs), also known as harvest strategies, across various states of nature, accounting for uncertainty. It is interactive and allows users to filter results live in order to explore robustness and performance.'
                                )),
                                p(i18n$t(
                                  'While Slick can be applied to any decision analysis context, it was specifically designed to investigate the performance of harvest strategies tested by management strategy evaluation (MSE). Slick can be used for any MSE process, but the current version includes a demonstration example, as well as preliminary results from the MSEs for Western Atlantic skipjack tuna and North Atlantic swordfish. Users can also upload custom Slick data files produced to display results from other MSEs (see the '), a(href='https://slick.bluematterscience.com/articles/DevelopersGuide.html', "Developer's Guide",  target="_blank"), ')')
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
                                    p(i18n$t('To use Slick, you need to either choose from one of the examples on the right, or upload a Slick Data file.')),
                                    p(i18n$t('Consult the'), a(href='https://slick.bluematterscience.com/', "Slick Homepage",  target="_blank"),
                                      i18n$t('for information on creating a Slick Data file.'))
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

    case_studies <- get_casestudies()

    output$load <- renderUI({
      options(shiny.maxRequestSize=200*1024^2)
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=3,
                                solidHeader=FALSE,
                                status = "primary",
                                title=strong(i18n$t('Load Slick Data')),
                                # uiOutput(ns('overviewlink')),
                                h4(i18n$t('Load your MSE Results')),
                                fileInput(ns("load"),
                                          accept=c("slick",".slick", 'rdat', '.rdat', 'rda', '.rda'),
                                          label = i18n$t("From file (.slick)"),
                                          buttonLabel=list(icon("folder",verify_fa = FALSE))
                                ),
                                h4(i18n$t('Load a Case Study')),
                                p(i18n$t('Select a case study and click the Load button')),
                                selectInput(ns('case_study_select'),
                                            label=i18n$t('Select Case Study'),
                                            choices=case_studies$Name,
                                            selected=NULL
                                ),
                                actionButton(ns("case_study_load"), i18n$t("Load"),
                                             icon("upload", verify_fa = FALSE)),
                                br(),
                                br(),
                                p(i18n$t('Click the Download button to download the Slick file')),
                                downloadButton(ns("case_study_download"), i18n$t("Download"),
                                                 icon("cloud-download", verify_fa = FALSE))
                                )
      )
    })

    loading_text <- reactive({
      ind <- match(input$case_study_select, case_studies$Name)
      paste0('Loading case study (', case_studies$Size[ind], 'Mb)')
    })

    w <- reactive({
      waiter::Waiter$new(
        color = "primary",
        html = waiter::attendantBar(
          "the-bar",
          width = 400,
          text = loading_text(),
        )
      )
    })

    att <- waiter::Attendant$new("the-bar")

    output$case_study_download <- downloadHandler(
      filename = function() {
        Name <- input$case_study_select
        paste0(Name, ".slick", sep="")
      },
      content = function(file) {
        slick <- download_casestudy(input$case_study_select, case_studies, quiet=FALSE)
        slick <- Update(slick)
        saveRDS(slick, file)
      }
    )

    observeEvent(input$load, ignoreInit = TRUE, {
      Load_Slick_File$file <- input$load
      Load_Slick_File$loaded <- Load_Slick_File$loaded + 1
    })

    observeEvent(input$case_study_load, ignoreInit = TRUE, {
      Load_Slick_File$file <- input$case_study_select
      Load_Slick_File$loaded <- Load_Slick_File$loaded +1
    })

    observeEvent(input$overview, {
      shinyjs::runjs("$('a[data-value=\"metadatatab\"]').tab('show');")
    })


    observeEvent(Load_Slick_File$loaded, ignoreInit = TRUE, {


      if (Load_Slick_File$loaded >= 1) {
        if (inherits(Load_Slick_File$file, 'character')) {
          w()$show()
          att$set(40)
          att$auto()

          slick <- download_casestudy(Load_Slick_File$file, case_studies, silent=TRUE)
          slick <- Update(slick)
          slick <- check_slick_file(slick)
          Slick_Object(slick)

          # jump to metadata tab
          shinyjs::delay(10,
                         shinyjs::runjs("$('a[data-value=\"metadatatab\"]').tab('show');")
          )
        }
        if (inherits(Load_Slick_File$file, 'data.frame')) {
          slick <- try(readRDS(Load_Slick_File$file$datapath))
          if (inherits(slick, 'try-error')) {
            shinyalert::shinyalert('Incorrect File Type',
                                   'Could not import file. Is it a Slick object created with `saveRDS?`',
                                   type='error')
          } else {
            slick <- check_slick_file(slick)
            Slick_Object(slick)

            # jump to metadata tab
            shinyjs::delay(10,
                           shinyjs::runjs("$('a[data-value=\"metadatatab\"]').tab('show');")
            )
          }
        }
      }

      on.exit({
        att$done()
        w()$hide()
      })
    })

  })
}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")

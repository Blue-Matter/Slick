#' Report_Page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Report_Page_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(3, uiOutput(ns('controls'))),
    column(9, uiOutput(ns('reportpreview')))

  )
}

#' Report_Page Server Functions
#'
#' @noRd
mod_Report_Page_server <- function(id, i18n, Slick_Object, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    # observe({
    #   rep_list <- reactiveValuesToList(Report)
    #   is_populated <- lapply(rep_list, lapply, length)
    #   is_populated$Metadata <- NULL
    #   Add_Report(sum(unlist(is_populated)))
    # })



    output$controls <- renderUI({
      i18n <- i18n()
      tagList(
        radioButtons(ns('format'), i18n$t('Report Format'),
                     choiceNames=c('HTML', 'PDF', 'MS Word'),
                     choiceValues=c('html', 'pdf', 'word')),
        downloadButton(ns("report"), "Generate report")
      )
    })

    output$reportpreview <- renderUI({
      filename()
      tagList(
        uiOutput(ns('metadata')),
        uiOutput(ns('quilt'))


      )
    })

    output$metadata <- renderUI({
      i18n <- i18n()
      tagList(
        h2(Report$Metadata$Title),
        Report$Metadata$Subtitle,
        Report$Metadata$Author,
        h3(i18n$t('Introduction')),
        markdown(Report$Metadata$Introduction)
      )
    })

    output$quilt <- renderUI({
      i18n <- i18n()
      if (length(Report$Quilt$plot)>0) {
        ll <- lapply(1:length(Report$Quilt$plot), function(x) {
          tagList(
            Report$Quilt$plot[[x]] |>
              flextable::autofit() |>
              flextable::htmltools_value(),
            Report$Quilt$caption[[x]]
          )
        })
      }
      tagList(
        h3(i18n$t('Quilt')),
        ll
      )

    })

    output_type <- reactive({
      req(input$format)
      paste0(input$format, '_document')
    })

    filename <- reactive({
      req(input$format)
      if (input$format=='word')
        return('Report.docx')
      paste0('Report.',input$format)
    })


    output$report <- downloadHandler(
      filename = filename(),
      content = function(file) {

        tempReport <- file.path(tempdir(), "Report_Template.Rmd")
        file.copy(file.path(app_sys(), "Report_Template.Rmd"),
                  tempReport, overwrite = TRUE)

        params <<- list(output=output_type(),
                        Metadata=Report$Metadata,
                        Quilt=Report$Quilt,
                        Tradeoff=Report$Tradeoff,
                        Spider=Report$Spider,
                        Zigzag=Report$Zigag)


        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

  })
}

## To be copied in the UI
# mod_Report_Page_ui("Report_Page_1")

## To be copied in the server
# mod_Report_Page_server("Report_Page_1")

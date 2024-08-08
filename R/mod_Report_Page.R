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
    # mod_toplink_ui(ns(id)),
    column(2, uiOutput(ns('controls'))),
    column(10, uiOutput(ns('reportpreview')))
  )
}

#' Report_Page Server Functions
#'
#' @noRd
mod_Report_Page_server <- function(id, i18n, Slick_Object, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # mod_toplink_server(id, links=list(hometab='Home',
    #                                   metadatatab='Overview',
    #                                   report='Report'))

    ready <- reactive({
      is_populated <- lapply(reactiveValuesToList(Report), lapply, length)
      is_populated$Metadata <- NULL
      sum(unlist(is_populated))
    })

    output$controls <- renderUI({
      i18n <- i18n()
      if (ready()>0) {
        tagList(
          radioButtons(ns('format'), i18n$t('Report Format'),
                       choiceNames=c('HTML', 'MS Word'),
                       choiceValues=c('.html', '.docx')),
          downloadButton(ns("report"), "Generate report")
        )
      }
    })

    output$reportpreview <- renderUI({
      i18n <- i18n()
      if (ready()>0) {
        tagList(
          uiOutput(ns('metadata')),
          uiOutput(ns('boxplot')),
          uiOutput(ns('quilt'))
        )
      } else {
        tagList(h3(i18n$t('Report feature coming soon!')))
        # tagList(h3(i18n$t('Nothing added to Report yet')))
      }

    })

    # Intro ----
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

    # Time Series ----


    # Boxplot ----

    boxplots <- reactive({
      nplot <- length(Report$Boxplot$plot)
      if (nplot>0) {
        plot_output_list <- lapply(1:nplot, function(x) {
          plotname <- paste0(x, 'boxplot')
          caption <<- Report$Boxplot$caption[[x]]
          if (!is.na(caption)) {
            caption <- p(caption)
          } else {
            caption <- NULL
          }
          if (!is.null(caption))
            list(
              plotOutput(ns(plotname)),
              caption,
              actionButton(ns(paste0('del-', plotname)), 'Delete')
            )
        })
        do.call('tagList', plot_output_list)
      }
    })

    observeEvent(Report$Boxplot, {
      nplot <- length(Report$Boxplot$plot)
      if (nplot>0) {
        for (x in 1:nplot) {
          local({
            this_x <- x
            this_plot <- Report$Boxplot$plot[[this_x]]
            if (!prod(is.na(this_plot)))
              output[[paste(this_x, "boxplot", sep="")]] <- renderPlot(this_plot)
          })
        }
      }
    })

    output$boxplot <- renderUI({
      TT <- lapply(Report$Boxplot$plot, is.na)
      if (all(!TT)) {
        tagList(
          h3('Boxplot'),
          boxplots()
        )
      }

    })


    observe({
      nplot <- length(Report$Boxplot$plot)
      if (nplot>0) {
        for (x in 1:nplot) {
          this_x <- x
          observeEvent(eventExpr = input[[paste0('del-', this_x, "boxplot")]],
                       handlerExpr = {
                         Report$Boxplot$plot[[this_x]] <- NA
                         Report$Boxplot$caption[[this_x]] <- NA
                       })
        }
      }
    })





    # Quilt ----
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
        tagList(
          h3('Quilt'),
          ll
        )
      }
    })

    extension <- reactive({
      req(input$format)
      input$format
    })

    output_format <- reactive({
      req(input$format)
      if (input$format=='.docx')
        return('word_document')
      paste0(sub('.', '', input$format), "_document")
    })


    output$report <- downloadHandler(
      filename = function() {
        paste0('Slick Report ', Sys.Date(), extension())
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "Report_Template.Rmd")
        file.copy(file.path(app_sys(), "Report_Template.Rmd"),
                  tempReport, overwrite = TRUE)

        params <- list(Metadata=Report$Metadata,
                        Quilt=Report$Quilt,
                        Tradeoff=Report$Tradeoff,
                        Spider=Report$Spider,
                        Zigzag=Report$Zigzag,
                        Kobe=Report$Kobe)

        rmarkdown::render(tempReport,
                          output_format = output_format(),
                          output_file = file,
                          params = params,
                          quiet = TRUE,
                          envir = new.env()
        )

      }
    )

  })
}

## To be copied in the UI
# mod_Report_Page_ui("Report_Page_1")

## To be copied in the server
# mod_Report_Page_server("Report_Page_1")

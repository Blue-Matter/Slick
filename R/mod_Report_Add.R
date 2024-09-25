#' Report_Add UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Report_Add_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('reportModal'))
  )
}

#' Report_Add Server Functions
#'
#' @noRd
mod_Report_Add_server <- function(id, i18n, parent_session, Report,
                                  Plot_Object=NULL, section=NULL,
                                  window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Plot_Info <- reactiveVal()

    output$reportModal <- renderUI({
      i18n <- i18n()

      modalDialog(
        size='l',
        title=i18n$t('Add to Report'),
        tagList(
          fluidRow(
            column(12,
                   uiOutput(ns('displayplot'))

            )
          ),
          fluidRow(
            column(9,
                   textAreaInput(ns('captionText'),
                                 i18n$t('Description'),
                                 placeholder=i18n$t('Description or caption for chart'),
                                 width='100%',
                                 height='150px'
                   )
            ),
            column(3,
                   h4(i18n$t('Image Settings')),
                   numericInput(ns('plotwidth'), 'Width  (inch)',
                                value=5,
                                min=1,
                                max=10),
                   numericInput(ns('plotheight'), 'Height (inch)',
                                value=5,
                                min=1,
                                max=10),
                   actionButton(ns("update_plot"), i18n$t("Update Plot"), icon=icon('redo'))
            )
          ),
          br()
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_new"), i18n$t("Add to Report"), icon=icon('pen'))
        )

      )
    })

    plot_size <- reactiveValues(width=5, height=5)

    observeEvent(input$update_plot, {
      p <- plot_details()
      if (!is.null(p))
        file.remove(p$src)

      plot_size$width <- input$plotwidth
      plot_size$height <- input$plotheight
      Plot_Info(plot_details())
    })


    plot_details <- reactive({
      obj <- Plot_Object()
      outfile <- tempfile(fileext='.png')

      info <- list(src = outfile,
                   contentType = 'image/png',
                   width = '800px',
                   height = '100%',
                   alt = "",
                   deleteFile=FALSE)

      if (inherits(obj, 'flextable')) {

        if (!requireNamespace('magick', quietly = TRUE))
          stop('Package `magick` required for this function')

        if (!requireNamespace('webshot2', quietly = TRUE))
          stop('Package `webshot2` required for this function')

        obj <- obj |>
          flextable::autofit() |>
          flextable::htmltools_value()

        tempfile <- tempfile(fileext='.html')
        kableExtra::save_kable(obj, tempfile)
        webshot2::webshot(tempfile, outfile, delay=3)
        file.remove(tempfile)
      } else {
        ggplot2::ggsave(outfile, obj, width=plot_size$width, height=plot_size$height)
      }

      info
    })

    observeEvent(Plot_Object(), {
      Plot_Info(plot_details())
    })

    output$figure <- renderImage(
      Plot_Info(), deleteFile = FALSE
      )

    output$displayplot <- renderUI({
      obj <<- Plot_Object()
      if (inherits(obj, 'flextable')) {
        TT <<- obj |>
          flextable::autofit() |>
          flextable::htmltools_value()
        print('done')
        return(TT)
      }

      t <- Plot_Info()
      if (!is.null(t)) {
        return(tagList(
          imageOutput(ns('figure'), width='800px', height='auto')
        )
        )
      } else {
        return(NULL)
      }
    })


    output$table <-renderUI({
      obj <- Plot_Object()
      if (inherits(obj, 'flextable')) {
        obj |>
          flextable::autofit() |>
          flextable::htmltools_value()
      }
    })

    output$plot <- renderPlot({
      obj <- Plot_Object()
      if (inherits(obj, 'flextable')) {
        return(NULL)
      }
      obj
    })


    caption <- reactive({
      input$captionText
    })

    observeEvent(input$save_new, {
      shiny::removeModal(parent_session)
      # update Report object with plot and caption
      Report[[section]]$plot <- c(Report[[section]]$plot, list(Plot_Info()))
      Report[[section]]$caption <- append(Report[[section]]$caption, caption())
    })

  })
}



## To be copied in the UI
# mod_Report_Add_ui("Report_Add_1")

## To be copied in the server
# mod_Report_Add_server("Report_Add_1")

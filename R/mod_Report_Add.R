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
  # passed to showModal(); only the contents are rendered by the server.
  # Rendering a modalDialog inside a uiOutput re-runs its show script and
  # leaves an extra backdrop behind, greying out the app after the modal closes
  modalDialog(
    size='l',
    title=uiOutput(ns('title'), inline=TRUE),
    uiOutput(ns('reportModal')),
    footer=uiOutput(ns('footer'))
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

    plot_size <- reactiveValues(width=5, height=5)

    output$title <- renderUI({
      i18n <- i18n()
      i18n$t('Add to Report')
    })

    output$footer <- renderUI({
      i18n <- i18n()
      tagList(
        div(modalButton(i18n$t('Cancel')), style='float:left;'),
        actionButton(ns("save_new"), i18n$t("Add to Report"), icon=icon('pen'))
      )
    })

    output$reportModal <- renderUI({
      i18n <- i18n()

      # tables size to their content, so no image settings
      is_table <- inherits(Plot_Object(), 'flextable')

      image_settings <- NULL
      if (!is_table)
        image_settings <- column(3,
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

      tagList(
        fluidRow(
          column(12,
                 uiOutput(ns('displayplot'))

          )
        ),
        fluidRow(
          column(ifelse(is_table, 12, 9),
                 textAreaInput(ns('captionText'),
                               i18n$t('Description'),
                               placeholder=i18n$t('Description or caption for chart'),
                               width='100%',
                               height='150px'
                 )
          ),
          image_settings
        ),
        br()
      )
    })

    observeEvent(input$plotwidth, {
      plot_size$width <- input$plotwidth
    })

    observeEvent(input$plotheight, {
      plot_size$height <- input$plotheight
    })


    # each new plot starts at the default size, matching the inputs
    observeEvent(Plot_Object(), {
      plot_size$width <- 5
      plot_size$height <- 5
      updateNumericInput(session, 'plotwidth', value=5)
      updateNumericInput(session, 'plotheight', value=5)
      Plot_Info(plot_details())
    })

    observeEvent(input$update_plot, {
      Plot_Info(plot_details())
    })


    # a function, not a reactive, so every call writes a new file; a cached
    # reactive would give two report items the same file
    plot_details <- function() {
      obj <- Plot_Object()
      outfile <- tempfile(fileext='.png')

      info <- list(src = outfile,
                   contentType = 'image/png',
                   width = '100%',
                   height = '100%',
                   alt = paste(section, 'plot added to the report'),
                   deleteFile=FALSE)

      if (inherits(obj, 'list')) {
        LL <- obj
        fun <- LL$plotFunction
        LL$plotFunction <- NULL

        grDevices::png(outfile,
                       width=plot_size$width,
                       height=plot_size$height,
                       units='in', res=400)
        do.call(fun,LL)
        grDevices::dev.off()

      } else if (inherits(obj, 'flextable')) {
        # tables size to their content, so width/height settings are ignored
        if (!requireNamespace('ragg', quietly = TRUE))
          stop('Package `ragg` required to save tables to the report')

        obj |>
          flextable::autofit() |>
          flextable::save_as_image(path=outfile, res=300)
      } else {
        ggplot2::ggsave(outfile,
                        obj,
                        width=plot_size$width,
                        height=plot_size$height)
      }
      info
    }

    output$figure <- renderImage(
      Plot_Info(), deleteFile = FALSE
      )

    output$displayplot <- renderUI({
      t <- Plot_Info()
      if (!is.null(t)) {
        return(tagList(
          imageOutput(ns('figure'), width='auto', height='400px')
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
    }, alt=function() {
      paste('Preview of the', section, 'plot to add to the report')
    })


    caption <- reactive({
      input$captionText
    })

    observeEvent(input$save_new, {
      shiny::removeModal(parent_session)
      Plot_Info(plot_details())
      Report[[section]]$plot <- c(Report[[section]]$plot, list(Plot_Info()))
      Report[[section]]$caption <- append(Report[[section]]$caption, caption())
    })

  })
}



## To be copied in the UI
# mod_Report_Add_ui("Report_Add_1")

## To be copied in the server
# mod_Report_Add_server("Report_Add_1")

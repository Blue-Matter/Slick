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
                                  Plot_Object=NULL, section=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$reportModal <- renderUI({
      i18n <- i18n()

      modalDialog(
        size='l',
        title=i18n$t('Add to Report'),
        tagList(
          column(12,
                 uiOutput(ns('image'))

          ),
          column(12,
                 textAreaInput(ns('captionText'),
                               i18n$t('Description'),
                               placeholder=i18n$t('Description or caption for chart'),
                               width='100%',
                               height='150px'
                 )
          )

        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_new"), "Add", icon=icon('pen'))
        )

      )
    })

    output$image <- renderUI({
      obj <- plot_object()
      if (inherits(obj, 'flextable')) {
        return(uiOutput(ns('table')))
      }
      plotOutput(ns('plot'))
    })

    output$table <-renderUI({
      obj <- plot_object()
      if (inherits(obj, 'flextable')) {
        obj |>
          flextable::autofit() |>
          flextable::htmltools_value()
      }
    })

    output$plot <- renderPlot({
      obj <- plot_object()
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
      Report[[section]]$plot <- c(Report[[section]]$plot, list(plot_object()))
      Report[[section]]$caption <- append(Report[[section]]$caption, caption())


    })

  })
}



## To be copied in the UI
# mod_Report_Add_ui("Report_Add_1")

## To be copied in the server
# mod_Report_Add_server("Report_Add_1")

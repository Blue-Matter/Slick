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
mod_Report_Add_server <- function(id, i18n, parent_session, Report, plot_object=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$reportModal <- renderUI({
      i18n <- i18n()
      modalDialog(
        size='l',
        title=i18n$t('Add to Report'),
        tagList(
          column(12,
                 plotOutput(ns('plot'))
          ),
          column(12,
                 textAreaInput(ns('captionText'),
                               i18n$t('Description'),
                               placeholder=i18n$t('Description or caption for chart'),
                               width='100%',
                               height='250px'
                 )
          )

        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_new"), "Add", icon=icon('pen'))
        )

      )
    })

    output$plot <- renderPlot({
      plot_object()
    })

    caption <- reactive({
      input$captionText
    })

    observeEvent(input$save_new, {
      shiny::removeModal(parent_session)
      print(caption())
    })

  })
}



## To be copied in the UI
# mod_Report_Add_ui("Report_Add_1")

## To be copied in the server
# mod_Report_Add_server("Report_Add_1")

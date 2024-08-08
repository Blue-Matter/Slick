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

    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.9
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

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
      obj <- Plot_Object()
      if (inherits(obj, 'flextable')) {
        return(uiOutput(ns('table')))
      }
      plotOutput(ns('plot'),
                 height='400px',
                 width=plot_width())
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
      Report[[section]]$plot <- c(Report[[section]]$plot, list(Plot_Object()))
      Report[[section]]$caption <- append(Report[[section]]$caption, caption())
    })

  })
}



## To be copied in the UI
# mod_Report_Add_ui("Report_Add_1")

## To be copied in the server
# mod_Report_Add_server("Report_Add_1")

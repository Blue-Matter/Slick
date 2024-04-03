


#' Quilt_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Quilt_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot'))
  )
}

#' Quilt_plot Server Functions
#'
#' @noRd
mod_Quilt_plot_server <- function(id, i18n, Slick_Object, Filter_Selected, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_quilt <- reactive({
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      selected_MPs <- Filter_Selected$MPs
      selected_PMs <- Filter_Selected$PMs
      quilt <- Quilt(slick)

      dd <- dim(Value(quilt))
      if (length(selected_OMs)==dd[1]) {
        # filter OMs
        if (!is.null(selected_OMs)) {
          Value(quilt) <- Value(quilt)[selected_OMs,,, drop=FALSE]
        }
        # filter MPs
        if (!is.null(selected_MPs)) {
          Value(quilt) <- Value(quilt)[,selected_MPs,, drop=FALSE]
        }
        # filter PMs
        if (!is.null(selected_PMs)) {
          Metadata(quilt) <- Metadata(quilt)[selected_PMs, ]
          Value(quilt) <- Value(quilt)[,,selected_PMs, drop=FALSE]
        }
      }

      quilt
    })

    nOM <- reactive({
      dim(Value(filtered_quilt()))[1]
    })

    MP_labels <- reactive({
      metadata <- Metadata(MPs(Slick_Object()))
      metadata$Code[Filter_Selected$MPs]
    })

    nMP <- reactive({
      length(MP_labels())
    })

    output$plot <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()

      tagList(
        br(),
        shinydashboard::box(width=12,
                            collapsible = TRUE,
                            status='primary',
                            title=strong(i18n$t("READING THIS CHART")),
                            uiOutput(ns('reading'))

        ),
        shinydashboard::box(width=12,
                            status='primary',
                            title=strong(paste(nMP(),
                                               i18n$t('Management Procedures. Median values over'),
                                               nOM(),
                                               i18n$t('Operating Models'))
                            ),
                            plotQuilt(filtered_quilt(), MP_labels(), i18n$get_translation_language())
        )
      )
    })

    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

    output$reading <- renderUI({
      i18n <- i18n()
      tagList(
        column(6,
               p(i18n$t('This table ...'))
               ),
        column(6,
               p(i18n$t('Use the'), actionLink(ns('openfilter'), i18n$t('Filter'), icon=icon('filter')),
                 i18n$t('button to filter the Management Procedures, Operating Models, and Performance Indicators ...')
               )
        )
      )
    })




  })
}

## To be copied in the UI
# mod_Quilt_plot_ui("Quilt_plot_1")

## To be copied in the server
# mod_Quilt_plot_server("Quilt_plot_1")

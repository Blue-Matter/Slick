


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
mod_Quilt_plot_server <- function(id, i18n, Slick_Object, Filter_Selected,
                                  parent_session,
                                  Report){
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


    mod_subtitle_server(id, i18n, nOM, nMP)

    output$plot <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()

      tagList(
        br(),
        column(12, mod_subtitle_ui(ns(id))
               ),
        column(2,
               h4(strong(i18n$t("Reading this Chart"))),
               htmlOutput(ns('reading'))
        ),
        column(10,
               shinyWidgets::actionBttn(ns('add_to_report'),
                                        label=i18n$t('Add to Report'),
                                        icon('pen'),
                                        color='default',size='sm'),
               plotQuilt(filtered_quilt(), MP_labels(), i18n$get_translation_language())
        )
      )
    })

    observeEvent(input$add_to_report, {
      # modal
      i18n <- i18n()
      this_quilt <- plotQuilt(filtered_quilt(),
                              MP_labels(),
                              i18n$get_translation_language(), TRUE)

      Report$Quilt$plot <- list(this_quilt)

      Report$Quilt$caption <- append(Report$Quilt$caption, 'This is the caption')

      OUT <<- Report$Quilt
    })

    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

    output$reading <- renderUI({
      i18n <- i18n()
      tagList(
        p(i18n$t('This table ...')),

        p(i18n$t('Use the'), actionLink(ns('openfilter'), i18n$t('Filter'), icon=icon('filter')),
          i18n$t('button to filter the Management Procedures, Operating Models, and Performance Indicators ...')
        )
      )
    })




  })
}

## To be copied in the UI
# mod_Quilt_plot_ui("Quilt_plot_1")

## To be copied in the server
# mod_Quilt_plot_server("Quilt_plot_1")

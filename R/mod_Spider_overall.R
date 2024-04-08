#' Spider_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('overall_Spider'))
  )
}

#' Spider_overall Server Functions
#'
#' @noRd
mod_Spider_overall_server <- function(id, i18n, filtered_slick,
                                      nOM, nMP, nPM, parent_session,
                                      relative_scale=relative_scale,
                                      window_dims){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

    output$overall_Spider <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          column(3,
                 h4(strong(i18n$t("Reading this Chart"))),
                 htmlOutput(ns('readingMP')),
          ),
          column(6,
                 uiOutput(ns('overallscore')),
                 shinycssloaders::withSpinner(
                   plotOutput(ns('spider_plot'), height=plot_height())
                 )
          ),
          column(3,
                 h4(i18n$t('Management Procedures')),
                 htmlOutput(ns('MPlist')),

                 h4(i18n$t('Performance Indicators')),
                 shinycssloaders::withSpinner(plotOutput(ns('PM_outline'), width=125, height=125)),
                 htmlOutput(ns('PMlist'))
          )
        )
      )
    })

    plot_height_calc <- reactive({
      dims <- window_dims()
      dims[1]*0.4
    })

    plot_height <- plot_height_calc |> debounce(500)

    output$spider_plot <- renderPlot({
      if (!is.null(relative_scale()))
        Spiderplot_all_MPs(filtered_slick(), relative_scale=relative_scale())
    }, width=function() {
      dims <- window_dims()
      plot_height()
    }, height=function() {
      plot_height()
    })

    output$MPlist <- renderUI({
      slick <- filtered_slick()
      metadata <- slick |> MPs() |> Metadata()
      nMP <- nrow(metadata)
      icon_text <- paste('<i class="fas fa-hexagon fa-sm" style="color:',
                         metadata$Color, ';"></i>', '  ',metadata$Label, '<br/>')
      icon_text <- paste(icon_text, collapse=" ")

      if (nMP>0) {
        tagList(
          p(
            HTML(icon_text)
          )
        )
      }
    })

    output$PM_outline <- renderPlot({
      if (nPM()>2) {
        pm_outline_plot(nPM())
      }
    }, width=125, height=125)

    output$PMlist <- renderUI({
      n.PM <- nPM()
      metadata <- Metadata(Spider(filtered_slick()))
      PM.name <- metadata$Label
      lets <- LETTERS[1:n.PM]

      icon_shape <- paste('<span class="circle"">', lets, '</span>')
      if (n.PM >2) {
        text <- rep('', n.PM)
        for (i in 1:n.PM) {
          text[i] <- paste(icon_shape[i], PM.name[i])
        }
        tagList(
          p(HTML(paste(text, collapse="<br>")), width='100%')
        )
      }
    })

    output$readingMP <- renderUI({
      i18n <- i18n()
      if (nMP()>0 & nPM()>0 & nOM()>0) {
        if (nPM()<3) {
          tagList(p(i18n$t('Please select 3 or more Performance Indicators')))
        } else {
          tagList(
            p(i18n$t('This chart'),
              strong(i18n$t('compares the performance of '), nMP(),
                     i18n$t(' management procedures (MP) against '), nPM(),
                     i18n$t(' performance indicators.'))),
            p(i18n$t('Each value is the median performance indicator over '), nOM(),
              i18n$t(' operating models.')),

            p(HTML('<i class="far fa-hexagon"></i>'),
              strong(i18n$t('The lines in the spider plot')),
              i18n$t('connect'), strong(i18n$t('individual scores')),
              i18n$t('of the performance indicators for each management procedure. Scores closer to the exterior edge indicate better performance.')
            ),
            p(i18n$t('Use the'), actionLink(ns('openfilter'),
                                            i18n$t('Filter'),
                                            icon=icon('filter')),
              i18n$t('button to filter the Management Procedures, Operating Models, and Performance Indicators.')
            )
          )
        }
      }
    })

  })
}

## To be copied in the UI
# mod_Spider_overall_ui("Spider_overall_1")

## To be copied in the server
# mod_Spider_overall_server("Spider_overall_1")

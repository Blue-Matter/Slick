pm_outline_plot <- function(n.PM) {
  par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(0,0,0,0))
  line.col <- 'darkgrey'
  pt.cex <- 3

  vertices <- polyCoords(n.PM) * 100
  plot(vertices, type="l", col=line.col, axes=FALSE, xlab="", ylab="", xpd=NA)
  lines(vertices*0.66, type="l", col=line.col, xpd=NA)
  lines(vertices*0.33, type="l", col=line.col, xpd=NA)
  for (i in 1:(nrow(vertices)-1)) {
    lines(x=c(0, vertices[i,1]),
          y=c(0, vertices[i,2]), col=line.col)
    points(x=vertices[i,1], y=vertices[i,2], pch=16, col=line.col, cex=pt.cex, xpd=NA)
    text(x=vertices[i,1], y=vertices[i,2], col='white', LETTERS[i], xpd=NA)
  }
}

#' Spider_MP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_MP_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('MP_Spider'))
  )
}

#' Spider_MP Server Functions
#'
#' @noRd
mod_Spider_MP_server <- function(id, i18n, filtered_slick, nOM, nMP, nPM, parent_session,
                                 relative_scale){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })


    output$MP_Spider <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
        column(3,
               h4(strong(i18n$t("Reading this Chart"))),
               htmlOutput(ns('readingMP')),
               br(),
               uiOutput(ns('OSbutton'))
        ),
        column(9,
               column(9,
                      uiOutput(ns('overallscore')),
                      shinycssloaders::withSpinner(
                        plotOutput(ns('filled_hex'), height='auto')
                      )
               ),
               column(3,
                      h4(i18n$t('Performance Indicators')),
                      shinycssloaders::withSpinner(plotOutput(ns('PM_outline'), width=125, height=125)),
                      htmlOutput(ns('PMlist'))
               )

        )
      )
      )
    })

    output$PM_outline <- renderPlot({
      if (nPM()>2) {
        pm_outline_plot(nPM())
      }
    }, width=125, height=125)

    output$overallscore <- renderUI({
      i18n <- i18n()
      if (is.null(input$OS_button)) {
        return(br())
      }
      if (input$OS_button) {
        h4(strong(i18n$t('Overall scores')), i18n$t('(average of '), nPM(),
           i18n$t('performance indicators)'))
      } else {
        br()
      }
    })

    output$filled_hex <- renderPlot({
      if (nPM()<3)
        return(NULL)
      include_avg <- input$OS_button
      if (!is.null(relative_scale()) & !is.null(include_avg)) {
        Spiderplot(filtered_slick(),
                   lang=i18n()$get_translation_language(),
                   relative_scale=relative_scale(),
                   include_avg=include_avg)
      }

    }, height=function(){
      nMPs <- filtered_slick() |> MPs() |> Metadata() |> nrow()
      if (nMPs<1) return(175)
      n.row <- ceiling(nMPs/4)
      175*n.row
    }, width=function(){
      nMPs <- filtered_slick() |> MPs() |> Metadata() |> nrow()
      if (nMPs<1) return(175)
      n.row <- ceiling(nMPs/4)
      n.col <- ceiling(nMPs/n.row)
      175*n.col
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
                     i18n$t(' performance metrics.'))),
            p(i18n$t('Each value is a performance metric over '), nOM(),
              i18n$t(' operating models.')),
            p(img(src='www/img/hexagon_solid.png', width="16", height="16"),
              i18n$t('The'), strong(i18n$t('filled plots')),
              i18n$t('represent an average score of all performance indicators for each management procedure. It provides a quick comparison of overall MP performances. '),
              strong(i18n$t('Larger areas indicate better overall performance')),
              p(i18n$t('These summary values'), strong(i18n$t('assume equal weighting and equal scaling of performance indicators.'))),
              p(i18n$t('Use the'), actionLink(ns('openfilter'),
                                              i18n$t('Filter'),
                                              icon=icon('filter')),
                i18n$t('button to filter the Management Procedures, Operating Models, and Performance Indicators.')
              )
            )
          )
        }
      }
    })

    output$OSbutton <- renderUI({
      i18n <- i18n()
      tagList(
        h4(i18n$t('Overall Score')),
        shinyWidgets::switchInput(
          inputId = ns("OS_button"),
          handleWidth = 80, labelWidth = 40,
          inline = TRUE, width = "130px",
          value=TRUE)
      )
    })

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


  })
}

## To be copied in the UI
# mod_Spider_MP_ui("Spider_MP_1")

## To be copied in the server
# mod_Spider_MP_server("Spider_MP_1")

#' Spider_OM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_OM_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('OM_Spider'))
  )
}

#' Spider_OM Server Functions
#'
#' @noRd
mod_Spider_OM_server <- function(id, i18n, filtered_slick,
                                 nOM, nMP, nPM, parent_session,
                                 relative_scale=relative_scale){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

    output$OM_Spider <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          column(3,
                 h4(strong(i18n$t("Reading this Chart"))),
                 htmlOutput(ns('reading')),
                 br(),
                 uiOutput(ns('OSbutton'))
          ),
          column(9,
                 column(9,
                        uiOutput(ns('results'), height='650px')
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
      )
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


    output$reading <- renderUI({
      i18n <- i18n()
      if (nMP()>0 & nPM()>0 & nOM()>0) {
        if (nPM()<3) {
          tagList(p(i18n$t('Please select 3 or more Performance Indicators')))
        } else {
          tagList(
            p(i18n$t('This chart'),
              strong(i18n$t('compares the performance of ')), nMP(),
              strong(i18n$t(' management procedures (MP)')),
              i18n$t('against '), nPM(),
              strong(i18n$t(' performance indicators')),
              i18n$t(' for a set of '), nOM(),
              strong(i18n$t(' operating models (columns).'))
            ),

            img(src='www/img/SpiderOM.jpg', width="100%"),

            p(tags$i(class="fa-solid fa-hexagon"),
              i18n$t('The polygon edges in each chart connect the individual scores of the performance indicators for that management procedure. Points closer to the exterior edge indicate better performance.')),
            p(i18n$t('The percentages represent an average score of all performance indicators for each management procedure. It provides a quick comparison of overall performance for each MP. Filled hexagons with larger areas indicate better overall performance.')),
            p(i18n$t('For each operating model (in columns), the management procedures (in rows) are ordered from highest to lowest overall average score.')),

            p(strong(i18n$t('Note:')),
              i18n$t('These summary values assume equal weighting and equal scaling of performance indicators. Use the button to turn off the Overall Scores.')),

            p(i18n$t('Use the'), actionLink(ns('openfilter'),
                                            i18n$t('Filter'),
                                            icon=icon('fa-lg fa-filter', class='fa-regular')),
              i18n$t('button to filter the Management Procedures, Operating Models, and Performance Indicators.')
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

    output$results <- renderUI({
      i18n <- i18n()
      slick <<- filtered_slick()
      if (nMP() >=2) {
        if (nPM()<3) {
          tagList(
            h3(i18n$t('Please select 3 or more Peformance Indicators'))
          )
        } else {
          hgt <- paste0(90 * nMP(), 'px')
          plot_output_list <- lapply(1:nOM(), function(mm) {
            plotname <- paste("plot", mm, sep="")
            tagList(
              h4(mm, class='OM_name', style = "font-size:18px;"),
              div(
                shinycssloaders::withSpinner(plotOutput(ns(plotname),
                                                        width='90px', height=hgt)),
                style="padding-right:50px;  background-color: #f2f2f2;;"
              )
            )
          })
          plot_output_list$cellArgs <- list(
            style = "width: 100px;"
          )
          do.call(flowLayout , plot_output_list)
        }

      } else {
        tagList(
          h3(i18n$t('Please select 2 or more MPs'))
        )
      }
    })

    MP_metadata <- reactive({
      if (!is.null(filtered_slick()))
        filtered_slick() |> MPs() |> Metadata()
    })

    observe({
      if (!is.null(filtered_slick()) & !is.null(relative_scale())) {

        slick <- filtered_slick()
        Values <- slick |> Spider() |> Value()

        Values[!is.finite(Values)] <- NA


        if(relative_scale()) {
          for (i in 1:nPM()) {
            Values[,,i] <- normalize(Values[,,i]) * 100
          }
          Values[!is.finite(Values)] <- 100
        }

        for (i in 1:nOM()) {
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")
            output[[plotname]] <- renderPlot({
              Spiderplot_single_OM(Values[my_i,,], MP_metadata(), include_avg=input$OS_button)
            }, height=function(){
              90 * nMP()
            }, bg = "transparent")
          })
        }
      }
    })



  })
}

## To be copied in the UI
# mod_Spider_OM_ui("Spider_OM_1")

## To be copied in the server
# mod_Spider_OM_server("Spider_OM_1")

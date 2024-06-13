#' Spider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))

  )
}

#' Spider Server Functions
#'
#' @noRd
mod_Spider_server <- function(id, i18n, Slick_Object, window_dims, Report, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      quilt='Spider'))

    mod_subtitle_server('spidersubtitle', i18n, nOM, nMP, nPM, minPM=3,
                        OMtext=OMtext)

    OMtext <- reactive({
      if (input$plotselect != 'byom')
        return('over')
      return('show')
    })

    Filter_Selected <- mod_Page_Filter_server("spiderfilter",i18n, Slick_Object,
                                              slot='Spider', minPM=3, icon='hexagon',
                                              home_session=home_session)

    mod_Spider_MP_server("Spider_MP_1", i18n, filtered_slick,
                         nOM, nMP, nPM, parent_session,
                         relative_scale=relative_scale, OS_button)
    mod_Spider_OM_server("Spider_OM_1", i18n, filtered_slick,
                         nOM, nMP, nPM, parent_session,
                         relative_scale=relative_scale,
                         OS_button)

    mod_Spider_overall_server("Spider_overall_1",
                              i18n, filtered_slick,
                              nOM, nMP, nPM, parent_session,
                              relative_scale=relative_scale,
                              window_dims)


    output$page <- renderUI({

      chk <- Check(filtered_slick())
      if (chk@empty$Spider) {
        return(NULL)
      }

      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Spider'))),
                                column(12,
                                       mod_subtitle_ui(ns('spidersubtitle'))
                                ),
                                column(8,
                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns("plotselect"),
                                         choiceNames = c(i18n$t('Overall'),
                                                         i18n$t('By Management Procedure'),
                                                         i18n$t('By Operating Model')
                                         ),
                                         choiceValues=c('overall', 'bymp',  'byom'),
                                         justified = TRUE
                                       )
                                ),
                                column(4,
                                       div(
                                         helper2(
                                           shinyWidgets::switchInput(
                                             inputId = ns("RS_button"),
                                             handleWidth = 60,
                                             labelWidth = 100,
                                             size='large',
                                             inline=TRUE,
                                             label=i18n$t('Relative Scale'),
                                             width = "auto"
                                           ),
                                           content = get_relative_scale_md(),
                                           size='s'),
                                         style='float:right;')
                                ),
                                column(3,
                                       h4(strong(i18n$t("Reading this Chart"))),
                                       htmlOutput(ns('reading')),
                                       mod_Page_Filter_ui(ns("spiderfilter"))
                                       ),
                                column(6,
                                       conditionalPanel("input.plotselect=='overall'", ns=ns,
                                                        mod_Spider_overall_ui(ns("Spider_overall_1"))
                                       ),
                                       conditionalPanel("input.plotselect=='bymp'", ns=ns,
                                                        mod_Spider_MP_ui(ns("Spider_MP_1"))
                                       ),
                                       conditionalPanel("input.plotselect=='byom'", ns=ns,
                                                        mod_Spider_OM_ui(ns("Spider_OM_1"))
                                       )
                                      ),
                                column(3,
                                       h4(i18n$t('Performance Indicators')),
                                       shinycssloaders::withSpinner(plotOutput(ns('PM_outline'),
                                                                               width=125, height=125)),
                                       htmlOutput(ns('PMlist'))
                                       )
        )
      )
    })

    relative_scale <- reactive({
      input$RS_button
    })

    dims <- reactive({
      d <- filtered_slick() |>
        Spider() |>
        Value() |>
        dim()
    })

    nOM <- reactive({
      dims()[1]
    })

    nPM <- reactive({
      dims()[3]
    })

    nMP <- reactive({
      filtered_slick() |>
        MPs() |>
        Metadata() |>
        nrow()
    })

    filtered_slick <- reactive({
      FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  as.numeric(Filter_Selected$PMs),
                  'Spider')
    })

    get_relative_scale_md <- reactive({
      lang <- i18n()$get_translation_language()
      paste('relative_scale', lang, sep='_')
    })

    output$reading <- renderUI({
      tagList(
        conditionalPanel("input.plotselect=='overall'", ns=ns,
                         uiOutput(ns('readingoverall'))
        ),
        conditionalPanel("input.plotselect=='bymp'", ns=ns,
                         uiOutput(ns('readingMP'))
        ),
        conditionalPanel("input.plotselect=='byom'", ns=ns,
                         uiOutput(ns('readingOM'))
        ),
        conditionalPanel("input.plotselect!='overall'", ns=ns,
                         uiOutput(ns('OSbutton'))
        )
      )

    })

    output$readingoverall <- renderUI({
      i18n <- i18n()
      if (nMP()>0 & nPM()>0 & nOM()>0) {
        if (nPM()<3) {
          tagList(p(i18n$t('Please select 3 or more Performance Indicators')))
        } else {
          tagList(
            p(i18n$t('This chart compares the performance of '), nMP(),
                     i18n$t(' management procedures (MP) against '), nPM(),
                     i18n$t(' performance indicators.')),
            p(i18n$t('Each value is the median performance indicator over '), nOM(),
              i18n$t(' operating models.')),
            p(i18n$t('Larger polygon areas indicate better overall performance.'))


          )
        }
      }
    })

    output$readingMP <- renderUI({
      i18n <- i18n()
      if (nMP()>0 & nPM()>0 & nOM()>0) {
        if (nPM()<3) {
          tagList(p(i18n$t('Please select 3 or more Performance Indicators')))
        } else {
          tagList(
            p(i18n$t('This chart compares the performance of '), nMP(),
              i18n$t(' management procedures (MP) against '), nPM(),
              i18n$t(' performance indicators.')),
            p(i18n$t('Each value is the median performance indicator over '), nOM(),
              i18n$t(' operating models.')),
            p(HTML('<i class="fa-solid fa-hexagon"></i>'),
              i18n$t('The filled plots represent an average score of all performance indicators for each management procedure. It provides a quick comparison of overall MP performances. Larger areas indicate better overall performance')),
            p(i18n$t('These summary values assume equal weighting and equal scaling of performance indicators.'))

          )
        }
      }
    })

    output$readingOM <- renderUI({
      i18n <- i18n()
      if (nMP()>0 & nPM()>0 & nOM()>0) {
        if (nPM()<3) {
          tagList(p(i18n$t('Please select 3 or more Performance Indicators')))
        } else {
          tagList(
            p(i18n$t('This chart'),
              i18n$t('compares the performance of '), nMP(),
              i18n$t(' management procedures (MP)'),
              i18n$t('against '), nPM(),
              i18n$t(' performance indicators'),
              i18n$t(' for a set of '), nOM(),
              i18n$t(' operating models (columns).')),

            img(src='www/img/SpiderOM.jpg', width="100%"),

            p(tags$i(class="fa-solid fa-hexagon"),
              i18n$t('The polygon edges in each chart connect the individual scores of the performance indicators for that management procedure. Points closer to the exterior edge indicate better performance.')),
            p(i18n$t('The percentages represent an average score of all performance indicators for each management procedure. It provides a quick comparison of overall performance for each MP. Filled hexagons with larger areas indicate better overall performance.')),
            p(i18n$t('For each operating model (in columns), the management procedures (in rows) are ordered from highest to lowest overall average score.')),

            p(strong(i18n$t('Note:')),
              i18n$t('These summary values assume equal weighting and equal scaling of performance indicators. Use the button to turn off the Overall Scores.'))
          )

        }
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

    OS_button <- reactive({
      input$OS_button
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

  })
}



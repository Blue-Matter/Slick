#' Boxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_Boxplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    # mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))
  )
}

#' Boxplot Server Functions
#'
#' @noRd
mod_Boxplot_server <- function(id, i18n, Slick_Object, window_dims, Report, home_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # mod_toplink_server(id, links=list(hometab='Home',
    #                                   metadatatab='Overview',
    #                                   boxplot='Boxplot'))

    mod_Boxplot_overall_server("Boxplot_overall_1",
                               i18n, filtered_slick, plottype,
                               nOM, nMP, nPM, parent_session=session,
                               window_dims, Report)

    mod_Boxplot_OM_server("Boxplot_OM_1", i18n, filtered_slick, plottype,
                          nOM, nMP, nPM, parent_session=session,
                          window_dims, Report,
                          selected_oms=selected_oms)

    mod_subtitle_server(id, i18n, nOM, nMP, OMtext=OMtext)

    OMtext <- reactive({
      if (input$plotselect == 'overall')
        return('over')
      return('show')
    })

    Filter_Selected <- mod_Page_Filter_server("boxplotfilter",i18n, Slick_Object,
                                              slot='Boxplot', minPM=1,
                                              home_session=home_session)

    selected_oms <- reactive({
      as.numeric(Filter_Selected$OMs)
    })

    selected_plotselect <- reactive({
      slick <- Slick_Object()
      selected <- 'overall'
      if (!is.null(slick)) {
        selected <- Defaults(Boxplot(slick))[[1]]
        if (!selected %in% c('overall',  'byom'))
          selected <- 'overall'
      }
      selected
    })

    selected_plot_type <- reactive({
      slick <- Slick_Object()
      selected <- 'boxplot'
      if (!is.null(slick)) {
        selected <- Defaults(Boxplot(slick))[[2]]
        if (!selected %in% c('boxplot', 'violin', 'both'))
          selected <- 'boxplot'
      }
      selected
    })


    output$page <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      if(all(is.na(slick@Boxplot@Value))) {
        return(tagList('No values in object'))
      }

      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong('Boxplot')),
                                br(),
                                column(12, mod_subtitle_ui(ns(id))),

                                column(3,
                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns("plotselect"),
                                         choiceNames = c(i18n$t('Overall'),
                                                         i18n$t('By Operating Model')
                                         ),
                                         choiceValues=c('overall',  'byom'),
                                         selected=selected_plotselect()
                                       )
                                ),
                                column(9,

                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns('plottype'),
                                         choiceNames  = c('Boxplot',
                                                         'Violin',
                                                          'Both'),
                                         choiceValues = c('boxplot', 'violin', 'both'),
                                         checkIcon = list(
                                           yes = tags$i(class = "fa fa-check-square",
                                                        style = "color: steelblue"),
                                           no = tags$i(class = "fa fa-square-o",
                                                       style = "color: steelblue")),
                                         selected=selected_plot_type()

                                       )
                                ),
                                column(3,
                                       conditionalPanel("input.plotselect=='overall'", ns=ns,
                                                        uiOutput(ns("reading_overall"))
                                       ),
                                       conditionalPanel("input.plotselect=='byom'", ns=ns,
                                                        uiOutput(ns("reading_om"))
                                       ),
                                       p(i18n$t('All performance indicators are defined such that higher values mean better performance and lower values mean worse performance.')),


                                       conditionalPanel("input.plottype=='boxplot'", ns=ns,
                                                        tagList(
                                                          p(i18n$t('Results can also be shown as violin plots by selecting the button on the right.'))
                                                        )
                                       ),
                                       conditionalPanel("input.plottype!='boxplot'", ns=ns,
                                                        tagList(
                                                          p(i18n$t('Violin plots are similar to boxplots, except that they also show the probability density of data at different values. The width of the violin plot indicates the proportion of data points that are in each region of the plot; i.e., wide areas of the plot indicate a relatively large number of data points in that region, while narrow areas of the plot indicate few data points. The plots extend the full range of the data values.'))
                                                        )
                                       ),

                                       img(src='www/img/Boxplot.jpg', width='100%'),


                                       mod_Page_Filter_ui(ns("boxplotfilter"))


                                ),
                                column(9,
                                       # mod_Report_Add_Button_ui(ns('report_button')),
                                       conditionalPanel("input.plotselect=='overall'", ns=ns,
                                                        mod_Boxplot_overall_ui(ns("Boxplot_overall_1"))
                                       ),
                                       conditionalPanel("input.plotselect=='byom'", ns=ns,
                                                        mod_Boxplot_OM_ui(ns("Boxplot_OM_1"))
                                       )
                                )
        )
      )
    })


    output$reading_overall <- renderUI({
      i18n <- i18n()
      tagList(
        h4(strong(i18n$t("Reading this Chart"))),
        p(
          i18n$t('This chart compares the performance of '), nMP(),
          i18n$t(' management procedures (MP) across '), nOM(),
          i18n$t(' operating models.')
        )
      )
    })


    output$reading_om <- renderUI({
      i18n <- i18n()
      tagList(
        h4(strong(i18n$t("Reading this Chart"))),
        p(
          i18n$t('This chart compares the performance of '), nMP(),
          i18n$t(' management procedures (MP) for '), nOM(),
          i18n$t(' operating models.')
        )
      )
    })


    plottype <- reactive({
      match(input$plottype, c('boxplot', 'violin', 'both'))
    })

    observeEvent(Slick_Object(), {
      filtered_slick()
    })

    filtered_slick <- reactive({
      out <- FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  as.numeric(Filter_Selected$PMs),
                  'Boxplot')
      out
    })

    dims <- reactive({
      slick <- filtered_slick()
      slick |>
        Boxplot() |>
        Value() |>
        dim()
    })

    nOM <- reactive({
      dims()[2]
    })

    nPM <- reactive({
      dims()[4]
    })

    nMP <- reactive({
      filtered_slick() |>
        MPs() |>
        Metadata() |>
        nrow()
    })
  })
}

## To be copied in the UI
# mod_Boxplot_ui("Boxplot_1")

## To be copied in the server
# mod_Boxplot_server("Boxplot_1")

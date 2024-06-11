#' Kobe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Kobe_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))
  )
}

#' Kobe Server Functions
#'
#' @noRd
mod_Kobe_server <- function(id, i18n, Slick_Object, window_dims, Report, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      kobe='Kobe'))

    selected_quantile <- mod_Kobe_overall_server("Kobe_overall_1", i18n, filtered_slick,
                            plottype,
                            nOM, nMP, nPM, parent_session=session,
                            window_dims)

    mod_Kobe_time_server("Kobe_time_1", i18n, filtered_slick,
                         plottype,
                         nOM, nMP, nPM, parent_session=session,
                         window_dims)

    mod_subtitle_server(id, i18n, nOM, nMP, OMtext=OMtext)

    OMtext <- reactive(
      'over'
    )

    Filter_Selected <- mod_Page_Filter_server("kobefilter",i18n, Slick_Object,
                                              slot='Kobe', minPM=1, FALSE,
                                              button_description='OM Filters',
                                              home_session=home_session)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)
    mod_Report_Add_server("Report_Add_2", i18n, parent_session=session, Report, plot_object)

    filtered_slick <- reactive({
      FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  as.numeric(Filter_Selected$PMs),
                  'Kobe')
    })

    observeEvent(Slick_Object(), {
      filtered_slick()
    })

    dims <- reactive({
      d <- filtered_slick() |>
        Kobe() |>
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


    output$page <- renderUI({
      chk <- Check(filtered_slick())
      if (chk@empty$Kobe) {
        return(NULL)
      }

      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Kobe'))),
                                br(),
                                column(12, mod_subtitle_ui(ns(id))),
                                column(12,
                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns("plotselect"),
                                         choiceNames = c(i18n$t('Overall'),
                                                         i18n$t('Kobe Time')
                                         ),
                                         choiceValues=c('overall',  'kobetime')
                                       )
                                ),
                                column(3,
                                       conditionalPanel("input.plotselect=='overall'", ns=ns,
                                                        uiOutput(ns('reading_overall'))
                                       ),
                                       conditionalPanel("input.plotselect=='kobetime'", ns=ns,
                                                        uiOutput(ns('reading_time'))

                                       ),
                                       mod_Page_Filter_ui(ns("kobefilter"))
                                ),
                                column(9,
                                       conditionalPanel("input.plotselect=='overall'", ns=ns,
                                                        mod_Kobe_overall_ui(ns("Kobe_overall_1"))
                                       ),
                                       conditionalPanel("input.plotselect=='kobetime'", ns=ns,
                                                        mod_Kobe_time_ui(ns("Kobe_time_1"))
                                       )
                                )
        )
      )
    })


    output$reading_overall <- renderUI({
      i18n <- i18n()
      slick <- filtered_slick()
      mp_metadata <- slick |> MPs() |> Metadata()
      time_info <- slick |> Kobe() |> Time()
      yrs <- time_info[[1]]
      quant_text <- selected_quantile() * 100

      yr.txt <- paste0(min(yrs), '-', max(yrs))
      tagList(
        p(i18n$t('This chart compares trade-offs in'), nMP(),
          i18n$t(' management procedures for '), nOM(),
          i18n$t(' operating models by measuring two co-dependent performance metrics: fishing mortality (vertical axis) and biomass (horizontal axis)')
        ),
        p(
          HTML('<i class="fas fa-circle fa-sm"></i>'),
          i18n$t('The large colored dots represent the median value for the final year of the projection period: '),
          yr.txt),
        p(i18n$t('The small colored dots indicate the median value in the first year of the projections, and the colored lines show how the median values change over time.')
        ),
        p( i18n$t('The white dotted lines around dots are error bars. The default represents'),
           HTML(paste0(quant_text, 'th')),
           i18n$t('percentiles, but that can be changed using the "Percentiles" scale at the right. The error bars can be removed with the checkbox on the right side.')
        )
      )
    })

    output$reading_time <- renderUI({
      i18n <- i18n()
      tagList(
        p(i18n$t('This chart compares the projected median values for '), nOM(),
          i18n$t('operating models over time for '), nMP(),
          i18n$t('management procedures and shows the levels of uncertainty.')),
        p(i18n$t('Segments within each bar are another way of looking at the error bars in the Kobe plot. They show the percentage of runs that fall in each of the Kobe quadrants in each projection year.')
        ),
        img(src='www/img/KobeTime_corrected.jpg',
            style="width: 100%"
        )
      )
    })
  })
}

## To be copied in the UI
# mod_Kobe_ui("Kobe_1")

## To be copied in the server
# mod_Kobe_server("Kobe_1")

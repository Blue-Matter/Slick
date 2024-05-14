#' Timeseries UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Timeseries_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    shinydashboardPlus::box(width=12,
                            status='primary',
                            solidHeader=TRUE,
                            title=uiOutput(ns('title')),
                            br(),
                            column(12, mod_subtitle_ui(ns(id))),
                            column(12,
                                   uiOutput(ns('groupbuttons'))
                            ),
                            fluidRow(
                              column(3,
                                     uiOutput(ns('reading')),
                                     img(src='www/img/Line.jpg', width="100%"),
                                     uiOutput(ns('picker')),
                                     uiOutput(ns('yaxisrange')),
                                     mod_Page_Filter_ui(ns("timeseriesfilter"))
                              ),
                              column(9,
                                     uiOutput(ns('plots'))
                              )
                            )
    )
  )
}

#' Timeseries Server Functions
#'
#' @noRd
mod_Timeseries_server <- function(id, i18n, Slick_Object, window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      timeseries='Timeseries'))

    mod_subtitle_server(id, i18n, nOM, nMP)

    Filter_Selected <- mod_Page_Filter_server("timeseriesfilter",i18n, Slick_Object,
                                              slot='Timeseries', incPM=FALSE,
                                              icon='chart-line')

    mod_Timeseries_overall_server("Timeseries_overall_1",
                                  i18n, filtered_slick,
                                  pm_ind, yrange,
                                  window_dims)

    mod_Timeseries_byMP_server("Timeseries_byMP_1", i18n, filtered_slick,
                               pm_ind, yrange, nMP,
                               window_dims)

    mod_Timeseries_byOM_server("Timeseries_byOM_1", i18n, filtered_slick,
                               pm_ind, yrange, nOM,
                               window_dims)



    output$plots <- renderUI({

      tagList(
        conditionalPanel("input.plotselect=='overall'", ns=ns,
                         mod_Timeseries_overall_ui(ns("Timeseries_overall_1"))
        ),
        conditionalPanel("input.plotselect=='bymp'", ns=ns,
                         mod_Timeseries_byMP_ui(ns("Timeseries_byMP_1"))
        ),
        conditionalPanel("input.plotselect=='byom'", ns=ns,
                         mod_Timeseries_byOM_ui(ns("Timeseries_byOM_1"))
        )
      )
    })

    output$title <- renderUI({
      i18n <- i18n()
      h3(strong(i18n$t('Timeseries')))
    })

    output$groupbuttons <- renderUI({
      i18n <- i18n()
      shinyWidgets::radioGroupButtons(
        inputId = ns("plotselect"),
        choiceNames = c(i18n$t('Overall'),
                        i18n$t('By Management Procedure'),
                        i18n$t('By Operating Model')
        ),
        choiceValues=c('overall', 'bymp',  'byom')
      )
    })

    output$picker <- renderUI({
      i18n <- i18n()
      shinyWidgets::pickerInput(ns('selectSV'),
                                i18n$t('Select Variable'),
                                choices=pm_labels(),
                                selected=1)
    })

    output$yaxisrange <- renderUI({
      i18n <- i18n()
      sliderInput(ns('yaxis'),
                  i18n$t('Y-Axis Maximum'),
                  min=0,
                  max=ymax(),
                  value=yvalue())
    })

    pm_ind_select <- reactive({
      as.numeric(input$selectSV)
    })

    pm_ind <- pm_ind_select

    yrange_select <- reactive({
      input$yaxis
    })

    yrange <- yrange_select

    sims <- reactive({
      vals <- Slick_Object() |> Timeseries() |> Value()
      1:dim(vals)[1]
    })

    pm_labels <- reactive({
      df <-  Slick_Object() |> Timeseries() |> Metadata()
      ll <- list()
      for (i in 1:nrow(df)) {
        ll[[i]] <- i
      }
      names(ll) <- df$Label
      ll
    })

    filtered_slick <- reactive({
      FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  as.numeric(Filter_Selected$PMs),
                  'Timeseries')
    })

    values <- reactive({
      filtered_slick() |>
        Timeseries() |>
        Value()
    })

    upper_quantile <- reactive({
      val <- values()
      selected <- as.numeric(input$selectSV)
      if (length(selected)<1)
        selected <- 1
      quantile(val[,,,selected,], 0.95)
    })

    ymax <- reactive({
      roundUpNice(upper_quantile())
    })

    yvalue <- reactive({
      c(0,roundUpNice(upper_quantile()))
    })


    dims <- reactive({
      d <- values() |> dim()
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

    nsim <- reactive({
      dims()[1]
    })

    output$readingOverall <- renderUI({
      i18n <- i18n()
      if (nOM() ==1) {
        txt <- HTML(paste0(' for a single Operating Model.'))
      } else {
        txt <- HTML(paste0(' and across ', nOM(), ' Operating Models.'))
      }

      tagList(
               p(i18n$t('This chart shows a stock status variables over time, for '), nMP(),
                 i18n$t('management procedures and level of uncertainty across'), nsim(),
                 i18n$t('different simulation runs'), txt),
               # p('Target and limit reference points are shown in green and red, respectively, if they have been specified.'),
      )
    })

    output$readingMP <- renderUI({
      i18n <- i18n()

      if (nOM() ==1) {
        txt <- HTML(paste0(' for a single Operating Model.'))
      } else {
        txt <- HTML(paste0(' and across ', nOM(), ' Operating Models.'))
      }

      tagList(
        p(i18n$t('This chart shows a stock status variables over time, for '), nMP(),
          i18n$t('management procedures and level of uncertainty across'), nsim(),
          i18n$t('different simulation runs'), txt),
        # p('Target and limit reference points are shown in green and red, respectively, if they have been specified.'),

      )
    })

    output$readingOM <- renderUI({
      i18n <- i18n()
      tagList(
        p(i18n$t('This chart shows a stock status variables over time, for '), nMP(),
          i18n$t('management procedures and'), nOM(), i18n$t('Operating Models.')),
        p(i18n$t('The results are either shown as the median across'), nsim(),
          i18n$t('simulations, or for a specific simulation.'))
        # p('Target and limit reference points are shown in green and red, respectively, if they have been specified.'),
      )
    })

    output$reading <- renderUI({
      i18n <- i18n()
      tagList(
        h4(strong(i18n$t("Reading this Chart"))),
        conditionalPanel("input.plotselect=='overall'", ns=ns,
                         uiOutput(ns('readingOverall'))),
        conditionalPanel("input.plotselect=='bymp'", ns=ns,
                         uiOutput(ns('readingMP'))),
        conditionalPanel("input.plotselect=='byom'", ns=ns,
                         uiOutput(ns('readingOM')))
      )
    })

  })
}

## To be copied in the UI
# mod_Timeseries_ui("Timeseries_1")

## To be copied in the server
# mod_Timeseries_server("Timeseries_1")

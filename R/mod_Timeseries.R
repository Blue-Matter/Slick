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
    # mod_toplink_ui(ns(id)),
    shinydashboardPlus::box(width=12,
                            status='primary',
                            solidHeader=TRUE,
                            title=h3(strong('Time Series')),
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
                                     uiOutput(ns('y_range')),
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
mod_Timeseries_server <- function(id, i18n, Slick_Object, window_dims, Report,
                                  home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # mod_toplink_server(id, links=list(hometab='Home',
    #                                   metadatatab='Overview',
    #                                   timeseries='Timeseries'))

    mod_subtitle_server(id, i18n, nOM, nMP, OMtext=OMtext)

    OMtext <- reactive({
      req(input$plotselect)
      if (input$plotselect != 'byom')
        return('over')
      return('show')
    })

    Filter_Selected <- mod_Page_Filter_server("timeseriesfilter",i18n, Slick_Object,
                                              slot='Timeseries', incPM=FALSE,
                                              icon='chart-line',
                                              button_description='OM Filters',
                                              home_session=home_session)


    mod_Timeseries_overall_server("Timeseries_overall_1",
                                  i18n, filtered_slick,
                                  pm_ind, yrange,
                                  window_dims,
                                  Report,
                                  parent_session=session,
                                  includeQuants, includeLabels,
                                  includeHist)

    mod_Timeseries_byMP_server("Timeseries_byMP_1", i18n, filtered_slick,
                               pm_ind, yrange, nMP,
                               window_dims,
                               Report,
                               parent_session=session,
                               includeQuants, includeLabels,
                               includeHist)

    mod_Timeseries_byOM_server("Timeseries_byOM_1", i18n, filtered_slick,
                               pm_ind, yrange, nOM,
                               window_dims,
                               selected_oms=selected_oms,
                               Report,
                               parent_session=session,
                               includeQuants, includeLabels,
                               includeHist)


    output$plots <- renderUI({
      tagList(
        fluidRow(
          column(4,uiOutput(ns('includeQuantile'))),
          column(4,uiOutput(ns('includeLabels'))),
          column(4,uiOutput(ns('includeHist')))
        ),
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

    observeEvent(input$plotselect, {
      if (input$plotselect=='bymp') {

        shinyjs::disable("incLabels")
      } else {
        shinyjs::enable("incLabels")
      }
    })


    output$picker <- renderUI({
      PM_labels <- pm_labels()
      if (length(PM_labels)<1) return(NULL)
      i18n <- i18n()
      tagList(
        h3(i18n$t('Select Variable:')),
        shinyWidgets::radioGroupButtons(
          inputId = ns("selectSV"),
          choiceNames =names(PM_labels),
          choiceValues=as.numeric(PM_labels))
      )
    })

    output$y_range <- renderUI({
      i18n <- i18n()
      sliderInput(ns('yaxis'),
                  i18n$t('Y-Axis Range'),
                  min=0,
                  max=ymax(),
                  value=yvalue(),
                  step=stepvalue())
    })

    output$includeQuantile <- renderUI({
      i18n <- i18n()
      checkboxInput(ns('incQuantile'),
                    i18n$t('Include MP percentiles?'),
                    TRUE)
    })



    output$includeLabels <- renderUI({
      i18n <- i18n()
      checkboxInput(ns('incLabels'),
                    i18n$t('Include MP Labels?'),
                    TRUE)
    })

    output$includeHist <- renderUI({
      i18n <- i18n()
      checkboxInput(ns('incHist'),
                    i18n$t('Include Historical?'),
                    TRUE)
    })

    includeQuants <- reactive({
      input$incQuantile
      })

    includeLabels <- reactive({
      input$incLabels
    } )

    includeHist <- reactive({
      input$incHist
    })


    stepvalue <- reactive({
      if (ymax()<10)
        return(0.1)
      1
    })


    pm_ind_select <- reactive({
      as.numeric(input$selectSV)
    })

    pm_ind <- pm_ind_select

    yrange <- reactive({
      input$yaxis
    })

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

    selected_oms <- reactive({
      as.numeric(Filter_Selected$OMs)
    })

    filtered_slick <- reactive({
      FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  NULL,
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
      quantile(val[,,,selected,], 0.95, na.rm=TRUE)
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
          i18n$t('management procedures and'), nOM(), i18n$t('individual Operating Models.')),
        p(i18n$t('The results are shown for 1 operating model at a time, either as the median across'), nsim(),
          i18n$t('simulations, or for a specific simulation.')),
        p(i18n$t('Click the "Operating Model" dropdown in the top menu for details of each OM.'))
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

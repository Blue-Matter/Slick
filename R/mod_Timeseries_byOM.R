#' Timeseries_byOM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Timeseries_byOM_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('selections')),
    uiOutput(ns('page'))
  )
}

#' Timeseries_byOM Server Functions
#'
#' @noRd
mod_Timeseries_byOM_server <- function(id, i18n, filtered_slick,
                                       pm_ind, yrange, nOM,
                                       window_dims,
                                       selected_oms,
                                       Report, parent_session,
                                       includeQuants, includeLabels, includeHist){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()

    timeseries <- reactiveVal()

    observeEvent(filtered_slick(),
                 timeseries(filtered_slick()))


    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Time Series',
                          window_dims)


    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {
      Plot_Object(timeseriesplot())
      if(!inherits(Plot_Object(), 'NULL'))
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    })

    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.6
      paste0(val, 'px')
    })

    nMP <- reactive({
      req(filtered_slick())
      slick <- filtered_slick()
      length(slick@MPs@Code)
    })

    plot_height_calc <- reactive({
      nom <- nOM()
      nmp <- nMP()
      if (byMP()) {
        ncol <- min(nmp, 4)
        nrow <- ceiling(nmp/ncol)
      } else {
        ncol <- min(nom, 4)
        nrow <- ceiling(nom/ncol)
      }
      paste0(nrow*300, 'px')

    })

    plot_width <- plot_width_calc |> debounce(500)
    plot_height <- plot_height_calc |> debounce(500)


    output$page <- renderUI({
      loading_spinner(plotOutput(ns('timeseriesMP'),
                                 width='100%',
                                 height=plot_height()))
    })

    output$timeseriesMP <- renderPlot({
      timeseriesplot()
    })

    timeseriesplot <- reactive({
      if (is.null(timeseries()))
        return(NULL)
      if (is.null(pm_ind()))
        return(NULL)
      if (is.null(yrange()))
        return(NULL)

      plotTimeseries(timeseries(),
                     pm_ind(),
                     byMP=byMP(),
                     byOM=TRUE,
                     includeQuants =includeQuants(),
                     includeLabels =includeLabels(),
                     includeHist = includeHist(),
                     lang=i18n()$get_translation_language()) +
        ggplot2::coord_cartesian(ylim=yrange())


    })

    byMP <- reactive({
      out <- input$byMP
      if (length(out)<1)
        return(FALSE)
      out
    })

    values <- reactive({
      filtered_slick() |>
        Timeseries() |>
        Value()
    })

    sims <- reactive({
      vals <-values()
      1:dim(vals)[1]
    })

    output$selections <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          column(4,
                 radioButtons(ns('plotoption'),
                              i18n$t('Plot Option'),
                              choiceNames = i18n$t(c('Median', 'Individual Simulation')),
                              choiceValues= c('median', 'sim')
                              )
                 ),
          column(4,
                 checkboxInput(ns('byMP'),
                               i18n$t('by MP?'),
                               FALSE)
          ),
          column(4,
                 conditionalPanel("input.plotoption=='sim'", ns=ns,
                                  column(6,
                                         shinyWidgets::pickerInput(
                                           inputId = ns('selectsim'),
                                           label = i18n$t('Individual Simulation:'),
                                           choices = sims(),
                                           multiple = FALSE,
                                           width='fit'
                                         )
                                  ),
                                  column(6,
                                         shinyjs::hidden(
                                           shinyWidgets::actionBttn(ns('updateplot'),
                                                                    i18n$t('Update Plots'))
                                         )
                                  )
                 )
          )
        )
      )
    })


    observeEvent(input$plotoption, {
      if (input$plotoption != 'sim') {
        shinyjs::hide('updateplot')
        timeseries(filtered_slick())
      } else {
        shinyjs::show('updateplot')
      }
    })

    observeEvent(input$selectsim, {
      shinyjs::show('updateplot')
    })


    observeEvent(input$updateplot, {
      shinyjs::hide('updateplot')
      slick <- filtered_slick()
      sim <- as.numeric(input$selectsim)
      slick@Timeseries@Value <- slick@Timeseries@Value[sim,,,,,drop=FALSE]
      timeseries(slick)
    }, ignoreInit = TRUE)


  })
}

## To be copied in the UI
# mod_Timeseries_byOM_ui("Timeseries_byOM_1")

## To be copied in the server
# mod_Timeseries_byOM_server("Timeseries_byOM_1")

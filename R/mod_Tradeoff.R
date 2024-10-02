#' Tradeoff UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Tradeoff_ui <- function(id){
  ns <- NS(id)
  tagList(
    # mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))

  )
}

#' Tradeoff Server Functions
#'
#' @noRd
mod_Tradeoff_server <- function(id, i18n, Slick_Object, window_dims, Report,
                                home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=session,
                          Report,
                          Plot_Object=Plot_Object, 'Tradeoff',
                          window_dims)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {
      Plot_Object(tradeoffplot())

      if(!inherits(Plot_Object(), 'NULL'))
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    })



    Filter_Selected <- mod_Page_Filter_server("tradeofffilter",i18n, Slick_Object,
                                              slot='Tradeoff', incPM=FALSE,
                                              button_description='OM Filters',
                                              home_session=home_session)

    mod_subtitle_server(id, i18n, nOM, nMP, OMtext=OMtext)

    OMtext <- reactive('over')

    filtered_slick <- reactive({
      FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  as.numeric(Filter_Selected$PMs),
                  'Tradeoff')
    })

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong('Tradeoff')),
                                column(12,
                                       mod_subtitle_ui(ns(id))
                                ),
                                column(3,
                                       h4(strong(i18n$t("Reading this Chart"))),
                                       htmlOutput(ns('reading')),
                                       mod_Page_Filter_ui(ns("tradeofffilter"))
                                ),
                                column(6,
                                       mod_Report_Add_Button_ui(ns('report_button')),
                                       plotOutput(ns('tradeoffplot'),
                                                  height=plot_height_d(),
                                                  width=plot_width_d())
                                ),
                                column(3, uiOutput(ns('pmselection')))

        )
      )
    })

    output$reading <- renderUI({
      i18n <- i18n()
      tagList(

        p(i18n$t('This chart plots the tradeoffs between any two performance indicators (select on right) for '),
          nMP(), i18n$t('management procedures (MP).'))

      )
    })

    nOM <- reactive({
      dd <- filtered_slick() |> Tradeoff() |> Value() |> dim()
      dd[1]
    })

    filtered_MPs <- reactive({
      slick <- Slick_Object()
      Metadata(MPs(slick))[Filter_Selected$MPs,]
    })

    nMP <- reactive({
      nrow(filtered_MPs())
    })




    PM_codes <- reactive({
      slick <- filtered_slick()
      code <-  slick@Quilt@Code
      ll <- as.list(seq_along(code))
      names(ll) <- code
      LL <<- ll
      ll
    })


    initial_selected <- reactive({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        # return(Selected(Tradeoff(slick)))
      }
      NULL
    })

    initial_x <- reactive({
      # selected <- initial_selected()
      # if (length(selected>0)) {
      #   return(selected[1])
      # }
      1
    })

    initial_y <- reactive({
      # selected <- initial_selected()
      # if (length(selected>1)) {
      #   return(selected[2])
      # }
      2
    })

    output$pmselection <- renderUI({
      i18n <- i18n()
      pm_codes <- PM_codes()
      tagList(
        p(i18n$t('Select the Performance Indicators to show on the X and Y axes of the Tradeoff plot:')),
        shinyWidgets::pickerInput(
          inputId = ns('xPM'),
          label = i18n$t("X-Axis Performance Indicator"),
          selected=initial_x(),
          choices = pm_codes
        ),
        shinyWidgets::pickerInput(
          inputId = ns('yPM'),
          label = i18n$t("Y-Axis Performance Indicator"),
          selected=initial_y(),
          choices = pm_codes
        )
      )
    })


    plot_height <- reactive({
      dims <- window_dims()
      dims[1]*0.3
    })

    plot_height_d <- plot_height |> debounce(500)
    plot_width_d <- reactive({
      plot_height_d() * 1.25
    })


    tradeoffplot <- reactive({

      plotTradeoff(filtered_slick(),
                   as.numeric(input$xPM),
                   as.numeric(input$yPM))
    })

    output$tradeoffplot <- renderPlot({
      req(filtered_slick())
      req(filtered_MPs())
      req(input$xPM)
      req(input$yPM)
      tradeoffplot()
    }, width=function() {
      plot_width_d()
    }, height=function() {
      plot_height_d()
    })
  })
}

## To be copied in the UI
# mod_Tradeoff_ui("Tradeoff_1")

## To be copied in the server
# mod_Tradeoff_server("Tradeoff_1")

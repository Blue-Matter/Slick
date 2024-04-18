

#' TradeOff_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TradeOff_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot'))
  )
}

#' TradeOff_plot Server Functions
#'
#' @noRd
mod_TradeOff_plot_server <- function(id, i18n, Slick_Object, Filter_Selected, parent_session, window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # button_pushed <- mod_Report_Add_Button_server("report_button", i18n)
    # mod_Report_Add_server("Report_Add_2", i18n, parent_session=session, Report, plot_object)

    filtered_slick <- reactive({
      if (is.null(Slick_Object())) return(NULL)
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      selected_MPs <- Filter_Selected$MPs

      tradeoff <- Tradeoff(slick)

      dd <- dim(Value(tradeoff))
      if (length(selected_OMs)==dd[1]) {
        # filter OMs
        if (!is.null(selected_OMs)) {
          Value(tradeoff) <- Value(tradeoff)[selected_OMs,,, drop=FALSE]
        }
        # filter MPs
        if (!is.null(selected_MPs)) {
          Value(tradeoff) <- Value(tradeoff)[,selected_MPs,, drop=FALSE]
          metadata <- Metadata(MPs(slick))
          Metadata(MPs(slick)) <- metadata[selected_MPs,]
        }

      }
      Tradeoff(slick) <- tradeoff
      slick
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


    pm_metadata <- reactive({
      Metadata(Tradeoff(Slick_Object()))
    })

    PM_codes <- reactive({
      pm_metadata()[['Code']]
    })

    mod_subtitle_server(id, i18n, nOM, nMP)

    output$plot <- renderUI({
      i18n <- i18n()
      tagList(
        br(),
        column(12,
               mod_subtitle_ui(ns(id))
        ),
        column(3,
               h4(strong(i18n$t("Reading this Chart"))),
               htmlOutput(ns('reading'))
               ),
        column(6,
               mod_Report_Add_Button_ui(ns('report_button')),
               plotOutput(ns('tradeoffplot'), height=plot_height_d(), width=plot_width_d())
        ),
        column(3, uiOutput(ns('pmselection')))
      )
    })


    # observeEvent(button_pushed(), {
    #   shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    # })

    output$reading <- renderUI({
      i18n <- i18n()
      tagList(

        p('This chart plots the tradeoffs between two performance indicators for ',
          nMP(), ' management procedures (MP). ...'),

        p(i18n$t('Use the'), actionLink(ns('openfilter'), i18n$t('Filter'), icon=icon('fa-lg fa-filter', class='fa-regular')),
          i18n$t('button to filter the Management Procedures and Operating Models used in this plot.')
        )

      )
    })


    initial_selected <- reactive({
      slick <- Slick_Object()
      if (!is.null(slick)) {
        return(Selected(Tradeoff(slick)))
      }
      NULL
    })

    initial_x <- reactive({
      selected <- initial_selected()
      if (length(selected>0)) {
        return(selected[1])
      }
      PM_codes()[1]
    })

    initial_y <- reactive({
      selected <- initial_selected()
      if (length(selected>1)) {
        return(selected[2])
      }
      PM_codes()[2]
    })

    output$pmselection <- renderUI({
      i18n <- i18n()
      pm_codes <- PM_codes()
      tagList(
        p(i18n$t('Select the Performance Indicators to show on the X and Y axes of the Trade-Off plot:')),
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

    plot_object <- reactive({
      plotTradeoff(filtered_slick(), filtered_MPs(), input$xPM, input$yPM)
    })

    output$tradeoffplot <- renderPlot({
      plotTradeoff(filtered_slick(), filtered_MPs(), input$xPM, input$yPM)
    }, width=function() {
      plot_width_d()
    }, height=function() {
      plot_height_d()
    })


    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

  })
}

## To be copied in the UI
# mod_TradeOff_plot_ui("TradeOff_plot_1")

## To be copied in the server
# mod_TradeOff_plot_server("TradeOff_plot_1")

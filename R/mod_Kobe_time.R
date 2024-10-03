#' Kobe_time UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Kobe_time_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('page'))
  )
}

#' Kobe_time Server Functions
#'
#' @noRd
mod_Kobe_time_server <- function(id,i18n, filtered_slick,
                                 parent_session,
                                 xvar, yvar,
                                 window_dims,
                                 Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    nOM <- reactive({
      slick <- filtered_slick()
      if (!is.null(slick))
        nrow(slick@OMs@Design)
    })

    nMP <- reactive({
      slick <- filtered_slick()
      if (!is.null(slick))
        length(slick@MPs@Code)
    })

    nPM <- reactive({
      slick <- filtered_slick()
      if (!is.null(slick))
        length(slick@Kobe@Code)
    })

    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Kobe',
                          window_dims)


    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {
      Plot_Object(kobeplot())

      if(!inherits(Plot_Object(), 'NULL'))
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    })

    kobeplot <- reactive({
      slick <- filtered_slick()
      if (is.null(slick))
        return(NULL)
      plotKobe(slick, xvar(), yvar(), Time=TRUE)
    })


    output$page <- renderUI({
      req(filtered_slick())
      chk <- Check(filtered_slick())
      if (chk@empty$Kobe) {
        return(NULL)
      }
      i18n <- i18n()
      tagList(
        fluidRow(
          column(12,
                 loading_spinner(plotOutput(ns('results'),
                                            width=plot_width(),
                                            height=plot_height()))
          )
        )
      )
    })

    output$results <- renderPlot({
      kobeplot()
    })

    calc_width <- reactive({
      dd <- window_dims()
      dd[1] * 0.6
    })

    plot_width_calc <- reactive({
      paste0(calc_width(), 'px')
    })

    calc_height <- reactive({
      width <- calc_width()
      nmp <- nMP()
      ncol <- min(4, nOM())
      nrow <- ceiling(nmp/ncol)
      (width*0.75)/ncol * nrow
    })

    plot_height_calc <- reactive({
      paste0(calc_height(), 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)
    plot_height <- plot_height_calc |> debounce(500)






  })
}

## To be copied in the UI
# mod_Kobe_time_ui("Kobe_time_1")

## To be copied in the server
# mod_Kobe_time_server("Kobe_time_1")

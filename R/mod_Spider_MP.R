

#' Spider_MP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_MP_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('MP_Spider'))
  )
}

#' Spider_MP Server Functions
#'
#' @noRd
mod_Spider_MP_server <- function(id, i18n, filtered_slick, nOM, nMP, nPM,
                                 relative_scale, OS_button,
                                 window_dims,
                                 Report,
                                 parent_session){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Spider',
                          window_dims)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {
      LL <- list()
      LL$plotFunction <- plotSpider
      LL$byMP <- TRUE
      LL$slick <- filtered_slick()
      LL$relScale <- relative_scale()
      LL$incMean <- OS_button()
      LL$incMax <- FALSE
      Plot_Object(LL)
      if(!inherits(Plot_Object(), 'NULL')) {
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
      }
    })

    spiderPlot <- reactive({
      req(relative_scale)
      req(filtered_slick)
      plotSpider(filtered_slick(),
                 byMP=TRUE,
                 relScale=relative_scale(),
                 incMean=OS_button(),
                 incMax=FALSE)

    })

    output$MP_Spider <- renderUI({
      chk <- Check(filtered_slick())
      if (chk@empty$Spider) {
        return(NULL)
      }
      i18n <- i18n()
      tagList(
        uiOutput(ns('overallscore')),
        loading_spinner(
          plotOutput(ns('filled_hex'))
        )
      )
    })

    nMPs <- reactive({
      n <- filtered_slick() |> MPs() |> Metadata() |> nrow()
      if (n<1) return(1)
      n
    })

    plot_height_calc <- reactive({
      nmp <- nMPs()
      width <- plot_width_calc()
      nrow <- ceiling(nmp/4)
      ncol <- min(nmp,4)
      width/ncol * nrow

    })

    plot_width_calc <- reactive({
      dims <- window_dims()
      dims[1]*0.4
    })

    plot_height <- plot_height_calc |> debounce(500)

    plot_width <- plot_width_calc |> debounce(500)


    output$overallscore <- renderUI({
      i18n <- i18n()
      if (is.null(OS_button())) {
        return(br())
      }
      if (OS_button()) {
        h4(strong(i18n$t('Overall scores')), i18n$t('(average of '), nPM(),
           i18n$t('performance indicators)'))
      } else {
        br()
      }
    })

    output$filled_hex <- renderPlot({
      if (nPM()<3) {
        print('nPM<3')
        return(NULL)
      }
      include_avg <- OS_button()
      if (!is.null(relative_scale()) & !is.null(include_avg)) {
        spiderPlot()
      }

    }, height=function() {
      plot_height()
      },
    width=function() {
      width=plot_width()
    } )

  })
}

## To be copied in the UI
# mod_Spider_MP_ui("Spider_MP_1")

## To be copied in the server
# mod_Spider_MP_server("Spider_MP_1")

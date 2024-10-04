#' Spider_OM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_OM_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_Report_Add_Button_ui(ns('report_button')),
    br(),
    uiOutput(ns('OM_Spider'))
  )
}

#' Spider_OM Server Functions
#'
#' @noRd
mod_Spider_OM_server <- function(id, i18n, filtered_slick,
                                 nOM, nMP, nPM,
                                 relative_scale=relative_scale, OS_button,
                                 selected_oms,
                                 Report,
                                 parent_session,
                                 window_dims){
  moduleServer( id, function(input, output, session){
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
      LL$slick <- filtered_slick()
      LL$relScale <- relative_scale()
      LL$incMean <- OS_button()
      LL$byOM <- TRUE
      LL$incMax <- FALSE
      Plot_Object(LL)
      if(!inherits(Plot_Object(), 'NULL')) {
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
      }
    })

    output$OM_Spider <- renderUI({
      i18n <- i18n()
      tagList(
        uiOutput(ns('results'), height='650px')
      )
    })

    output$results <- renderUI({
      i18n <- i18n()
      slick <- filtered_slick()
      if (nMP() >=2) {
        if (nPM()<3) {
          tagList(
            h3(i18n$t('Please select 3 or more Peformance Indicators'))
          )
        } else {
          plot_output_list <- lapply(1:nOM(), function(mm) {
            plotname <- paste("plot", mm, sep="")
            tagList(
              loading_spinner(plotOutput(ns(plotname),
                                         width=plot_width(), height=plot_height()))

            )
          })
          plot_output_list$cellArgs <- list(
            style = paste0('width:', cell_width_calc(), '; padding-bottom:25px;')
          )
          do.call(flowLayout , plot_output_list)
        }
      } else {
        tagList(
          h3(i18n$t('Please select 2 or more MPs'))
        )
      }
    })

    MP_metadata <- reactive({
      if (!is.null(filtered_slick()))
        filtered_slick() |> MPs() |> Metadata()
    })

    width <- reactive({
      dd <- window_dims()
      dd[1] * 0.1
    })

    plot_height_calc <- reactive({
      req(width, nMP)
      nMP() * width()
    })

    plot_height <- plot_height_calc |> debounce(500)

    plot_width_calc <- reactive({
      paste0(width(), 'px')
    })

    cell_width_calc <- reactive({
      paste0(width()+35, 'px')
    })


    plot_width <- plot_width_calc |> debounce(500)
    cell_width <- cell_width_calc |> debounce(500)


    observe({
      if (!is.null(filtered_slick()) & !is.null(relative_scale())) {

        slick <- filtered_slick()
        Values <- slick |> Spider() |> Value()

        Values[!is.finite(Values)] <- NA


        if(relative_scale()) {
          for (i in 1:nPM()) {
            Values[,,i] <- normalize(Values[,,i]) * 100
          }
          Values[!is.finite(Values)] <- 100
        }

        for (i in 1:nOM()) {
          local({
            my_i <- i
            plotname <- paste("plot", my_i, sep="")
            output[[plotname]] <- renderPlot({
              plotSpider(filtered_slick(), byOM=my_i,
                         relScale=relative_scale(),
                         incMean=OS_button(),
                         incMax=TRUE)
            }, height=function(){
              plot_height()
            }, bg = "transparent")
          })
        }
      }
    })


  })
}

## To be copied in the UI
# mod_Spider_OM_ui("Spider_OM_1")

## To be copied in the server
# mod_Spider_OM_server("Spider_OM_1")

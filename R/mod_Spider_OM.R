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
    uiOutput(ns('OM_Spider'))
  )
}

#' Spider_OM Server Functions
#'
#' @noRd
mod_Spider_OM_server <- function(id, i18n, filtered_slick,
                                 nOM, nMP, nPM, parent_session,
                                 relative_scale=relative_scale, OS_button,
                                 selected_oms){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
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
          hgt <- paste0(90 * nMP(), 'px')
          om_labels <- selected_oms()
          plot_output_list <- lapply(1:nOM(), function(mm) {
            plotname <- paste("plot", mm, sep="")
            tagList(
              h4(om_labels[mm], class='OM_name', style = "font-size:18px;"),
              div(
                loading_spinner(plotOutput(ns(plotname),
                                                        width='90px', height=hgt)),
                style="padding-right:50px;  background-color: #f2f2f2;;"
              )
            )
          })
          plot_output_list$cellArgs <- list(
            style = "width: 100px;"
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
              Spiderplot_single_OM(Values[my_i,,], MP_metadata(), include_avg=OS_button())
            }, height=function(){
              90 * nMP()
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

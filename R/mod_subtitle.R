#' subtitle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_subtitle_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('subtitle'))
  )
}

#' subtitle Server Functions
#'
#' @noRd
mod_subtitle_server <- function(id, i18n, nOM, nMP, nPM=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$subtitle <- renderUI({

      i18n <- i18n()

      if (!is.null(nPM)) {
        if (nPM() <=2) {
          return(
            tagList(
              div(class='page_title',
                  p(i18n$t('Please select 3 or more Performance Indicators'), id='subtitle',
                    style="color:red;")
              )
            )
          )
        }
      }


      if (nOM()>0 & nMP()>0) {
        str <- paste0(nMP(),
                      i18n$t(' Management Procedures. Median values over '), nOM(),
                      i18n$t(' Operating Models.'))
      } else {
        str <-''
      }
      tagList(
        div(class='page_title',
            p(str, id='subtitle')
        )
      )
    })

  })
}

## To be copied in the UI
# mod_subtitle_ui("subtitle_1")

## To be copied in the server
# mod_subtitle_server("subtitle_1")

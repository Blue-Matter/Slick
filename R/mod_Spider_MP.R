

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
    uiOutput(ns('MP_Spider'))
  )
}

#' Spider_MP Server Functions
#'
#' @noRd
mod_Spider_MP_server <- function(id, i18n, filtered_slick, nOM, nMP, nPM,
                                 relative_scale, OS_button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$MP_Spider <- renderUI({
      chk <- Check(filtered_slick())
      if (chk@empty$Spider) {
        return(NULL)
      }

      i18n <- i18n()
      tagList(
        uiOutput(ns('overallscore')),
        loading_spinner(
          plotOutput(ns('filled_hex'), height='auto')
        )
      )
    })

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
        Spiderplot(filtered_slick(),
                   lang=i18n()$get_translation_language(),
                   relative_scale=relative_scale(),
                   include_avg=include_avg)
      }

    }, height=function(){
      nMPs <- filtered_slick() |> MPs() |> Metadata() |> nrow()
      if (nMPs<1) return(175)
      n.row <- ceiling(nMPs/4)
      175*n.row
    }, width=function(){
      nMPs <- filtered_slick() |> MPs() |> Metadata() |> nrow()
      if (nMPs<1) return(175)
      n.row <- ceiling(nMPs/4)
      n.col <- ceiling(nMPs/n.row)
      175*n.col
    })


  })
}

## To be copied in the UI
# mod_Spider_MP_ui("Spider_MP_1")

## To be copied in the server
# mod_Spider_MP_server("Spider_MP_1")

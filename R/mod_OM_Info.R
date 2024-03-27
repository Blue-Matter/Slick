Make_OM_Factor_Table <- function(OMs, lang) {
  design <- Design(OMs)
  desc <- Description(OMs, lang)
  ll <- list()
  for (i in seq_along(desc)) {
    ll[[i]] <-   data.frame(Factor=colnames(design)[i],
                            Level=unique(design[,i]),
                            Description=desc[[i]])
  }
  df <- do.call('rbind', ll)
  DT::datatable(df,
                extensions = 'Responsive',
                selection='none',
                options = list(dom = 't',
                               pageLength=100,
                               ordering=F))
}

#' OM_Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_OM_Info_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('OMinfo'))
  )
}


#' OM_Info Server Functions
#'
#' @noRd
mod_OM_Info_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$OM_factors <- DT::renderDataTable({
      i18n <- i18n()
      slick <<- Slick_Object()
      Make_OM_Factor_Table(OMs(slick), i18n$get_translation_language())
    })

    output$OM_design <- DT::renderDataTable({
      slick <- Slick_Object()
      DT::datatable(Design(slick),
                    extensions = 'Responsive',
                    selection='none',
                    options = list(dom = 't',
                                   pageLength=100,
                                   ordering=F,
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"))))

    })

    output$OMinfo <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()

      tabsetPanel(type="tabs",
                  tabPanel(i18n$t('Factors'),
                           br(),
                           DT::dataTableOutput(ns('OM_factors'))),
                  tabPanel(i18n$t('Design'),
                           br(),
                           DT::dataTableOutput(ns('OM_design')))
      )
    })



  })
}

## To be copied in the UI
# mod_OM_Info_ui("OM_Info_1")

## To be copied in the server
# mod_OM_Info_server("OM_Info_1")

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
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))

  )
}

#' Tradeoff Server Functions
#'
#' @noRd
mod_Tradeoff_server <- function(id, i18n, Slick_Object, window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Tradeoff',
                                        parent_session=session)

    mod_TradeOff_plot_server("TradeOff_plot_1", i18n, Slick_Object, Filter_Selected, parent_session=session, window_dims)

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      tradeoff='Tradeoff'))

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Tradeoff'))),
                                mod_TradeOff_plot_ui(ns("TradeOff_plot_1")),
                                sidebar = shinydashboardPlus::boxSidebar(id=ns('filtersidebar'),
                                                                         icon=icon('filter'),
                                                                         column(12, align = 'left', class='multicol',
                                                                                mod_Filter_ui(ns(id))
                                                                         )
                                )
        ) %>% {
          htmltools::tagQuery(.)$
            find("#filtersidebar")$
            removeAttrs("data-original-title")$
            addAttrs(`data-original-title`="Filters")$
            allTags()
        }
      )
    })

  })
}

## To be copied in the UI
# mod_Tradeoff_ui("Tradeoff_1")

## To be copied in the server
# mod_Tradeoff_server("Tradeoff_1")

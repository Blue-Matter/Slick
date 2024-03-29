

#' Quilt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Quilt_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))

  )
}

#' Quilt Server Functions
#' @importFrom dplyr %>%
#' @noRd
mod_Quilt_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Quilt',
                                        parent_session=session)

    mod_Quilt_plot_server("Quilt_plot_1", i18n, Slick_Object, Filter_Selected)
    mod_TradeOff_plot_server("TradeOff_plot_1", i18n, Slick_Object, Filter_Selected)

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      quilt='Quilt'))

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Quilt and Trade-Off'))),
                                shiny::tabsetPanel(id=ns('quilt_tabs'),
                                                   shiny::tabPanel('Quilt',
                                                                   mod_Quilt_plot_ui(ns("Quilt_plot_1"))
                                                   ),
                                                   shiny::tabPanel('Trade-Off',
                                                                   mod_TradeOff_plot_ui(ns("TradeOff_plot_1"))
                                                   )

                                ),
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
# mod_Quilt_ui("Quilt")

## To be copied in the server
# mod_Quilt_server("Quilt")
#' Spider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))

  )
}

#' Spider Server Functions
#'
#' @noRd
mod_Spider_server <- function(id, i18n, Slick_Object, window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Spider',
                                        parent_session=session)

    mod_Spider_plot_server("Spider_plot_1",
                           i18n,
                           Slick_Object,
                           Filter_Selected,
                           parent_session=session,
                           window_dims)


    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      quilt='Spider'))

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Spider'))),
                                mod_Spider_plot_ui(ns("Spider_plot_1")),
                                sidebar = shinydashboardPlus::boxSidebar(id=ns('filtersidebar'),
                                                                         icon=icon('filter'),
                                                                         column(12, align = 'left', class='multicol',
                                                                                mod_Filter_ui(ns(id))
                                                                         )
                                )
        )
      )
    })



  })
}



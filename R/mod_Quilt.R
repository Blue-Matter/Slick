

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
#' @noRd
mod_Quilt_server <- function(id, i18n, Slick_Object, window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Quilt',
                                        parent_session=session)

    mod_Quilt_plot_server("Quilt_plot_1", i18n, Slick_Object, Filter_Selected,
                          parent_session=session, Report)

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      quilt='Quilt'))

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Quilt'))),
                                mod_Quilt_plot_ui(ns("Quilt_plot_1")),
                                sidebar = shinydashboardPlus::boxSidebar(id=ns('filtersidebar'),
                                                                         icon=icon('fa-xl fa-filter', class='fa-regular'),
                                                                         column(12, align = 'left', class='multicol',
                                                                                mod_Filter_ui(ns(id))
                                                                                )
                                )
        )
      )
    })

  })
}

## To be copied in the UI
# mod_Quilt_ui("Quilt")

## To be copied in the server
# mod_Quilt_server("Quilt")

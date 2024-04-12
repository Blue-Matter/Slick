#' Boxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_Boxplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))

  )
}

#' Boxplot Server Functions
#'
#' @noRd
mod_Boxplot_server <- function(id, i18n, Slick_Object, window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      boxplot='Boxplot'))

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Boxplot',
                                        parent_session=session)

    mod_Boxplot_plot_server("Boxplot_plot_1",
                           i18n,
                           Slick_Object,
                           Filter_Selected,
                           parent_session=session,
                           window_dims)


    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Boxplot'))),
                                mod_Boxplot_plot_ui(ns("Boxplot_plot_1")),
                                sidebar = shinydashboardPlus::boxSidebar(id=ns('filtersidebar'),
                                                                         icon=icon('fa-xl fa-filter', class='fa-regular'),
                                                                         column(12, align = 'left', class='multicol',
                                                                                mod_Filter_ui(ns(id))
                                                                         )
                                )
        ) %>% { # not working for some reason
          htmltools::tagQuery(.)$
            find("filtersidebar")$
            removeAttrs("data-original-title")$
            addAttrs(`data-original-title`="Filters")$
            allTags()
        }
      )
    })



  })
}

## To be copied in the UI
# mod_Boxplot_ui("Boxplot_1")

## To be copied in the server
# mod_Boxplot_server("Boxplot_1")

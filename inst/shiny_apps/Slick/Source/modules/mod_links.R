mod_links_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('links'))
}

#' links Server Functions
#'
#' @noRd
mod_links_server <- function(id, Object, i18n){


  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$loaded_links <- renderUI({
      i18n <- i18n()
      if (Object$Loaded) {
        tagList(
          column(12,
                 shiny::actionLink(ns('metadata'),
                                   i18n$t('Overview'),
                                   icon=icon('info-circle'),
                                   width='100%')
                 )
        )
      } else {
        NULL
      }

    })

    output$links <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboard::box(title=strong(i18n$t('Links')), width=2,
                            solidHeader = TRUE,
                            status='primary',
                            column(12,
                                   shiny::actionLink(ns('home'),
                                                     i18n$t('Home'),
                                                     icon=icon('home'),
                                                     width='100%')
                                   ),
                            uiOutput(ns('loaded_links'))

        )
      )

    })




    make_link <- function(menuid) {
      js <- paste0('[data-value=\"', menuid, '\"]')
      shinyjs::runjs(paste0("$('a", js, "').tab('show');"))
    }

    observeEvent(input$home, {
      make_link('home')
    })

    observeEvent(input$metadata, {
      make_link('metadata')
    })


  })
}

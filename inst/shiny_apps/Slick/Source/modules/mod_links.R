mod_links_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('links'))
    # column(12, shiny::actionLink(ns('table'), 'Table', icon=icon('table'), width='100%')),
    #
    # column(12,strong('Scoring Pages')),
    # column(12,shiny::actionLink(ns('egg_alevin'), 'Egg/Alevin', icon=icon('egg', class='fa-sm'))),
    # column(12,shiny::actionLink(ns('fry_parr'), 'Fry/Parr',icon=icon('fish', class='fa-xs'))),
    # column(12,shiny::actionLink(ns('smolt'), 'Smolt',icon=icon('fish', class='fa-sm'))),
    # column(12,shiny::actionLink(ns('juvenile'), 'Juvenile',icon=icon('fish'))),
    # column(12,shiny::actionLink(ns('immature'), 'Immature',icon=icon('fish', class='fa-lg'))),
    # column(12,icon('fish', class='fa-xl', style='color: #3c8dbc;'),'Adult'),
    #
    # shiny::tags$ul(
    #   shiny::tags$li(shiny::actionLink(ns('return_migration'), 'Return Migration')),
    #   shiny::tags$li(shiny::actionLink(ns('terminal_migration'), 'Terminal Migration')),
    #   shiny::tags$li(shiny::actionLink(ns('spawning'), 'Spawning'))
    # ),
    #
    # column(12,strong('Other Resources')),
    # column(12,shiny::actionLink(ns('go_link_1'), 'Link 1')),
    # column(12,shiny::actionLink(ns('go_link_2'), 'Link 2')),
    # column(12,'These links can be specific to each page')

  # )
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


get_language <- function(txt, i18n) {
  if (inherits(txt, 'list')) {
    lang <- i18n$get_translation_language()
    txt <- txt[[lang]]
  }
  txt
}

Fishery_Text <- function(slick, i18n) {
  txt <- get_language(Fishery(slick),i18n)
  if (length(txt)<1) return(NULL)
  p(strong(i18n$t('Fishery:')), txt)
}


#' Metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Metadata_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('main'))
  )
}

#' Metadata Server Functions
#'
#' @noRd
mod_Metadata_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$main <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboard::box(title=h3(strong(i18n$t('Overview'))),
                                width=12,
                                solidHeader=TRUE,
                                status = "primary",
                                uiOutput(ns('tabsetpanel'))
        )
        # mod_links_ui(ns(id))
      )
    })

    # main tabBox panels ----
    output$tabsetpanel <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          shinydashboard::tabBox(width=12,
                                 tabPanel(title=h4(strong(i18n$t('Metadata'))),
                                          uiOutput(ns('metadata'))

                                 ),
                                 tabPanel(title=h4(strong(i18n$t('Management Procedures'))),
                                          uiOutput(ns('management_procedures'))

                                 ),
                                 tabPanel(title=h4(strong(i18n$t('Operating Models'))),
                                          uiOutput(ns('operating_models'))

                                 ),
                                 tabPanel(title=h4(strong(i18n$t('Performance Metrics'))),
                                          uiOutput(ns('performance_metrics'))

                                 )
          )
        )
      )
    })

    # ---- metadata tab ----
    output$metadata <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()

      tagList(

        column(12,
               br(),
               h3(get_language(Title(slick), i18n)),
               h4(get_language(Subtitle(slick), i18n)),
               br(),
               Fishery_Text(slick, i18n)


               # p(strong(i18n$t('Author(s):')), Object$obj$Misc$Author, HTML(Object$obj$Misc$Contact)),
               # p(strong(i18n$t('Institution(s):')), Object$obj$Misc$Institution),
               # p(strong(i18n$t('Created:')), Object$obj$Misc$Date),
               # lapply(lapply(Object$obj$Text$Introduction, HTML), tags$p)
        )
      )
    })


  })
}

## To be copied in the UI
# mod_Metadata_ui("Metadata_1")

## To be copied in the server
# mod_Metadata_server("Metadata_1")

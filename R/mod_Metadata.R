
get_authors <- function(slick) {
  authors <- Author(slick)
  if (length(authors)>1)
    return(paste(paste0(1:length(authors), '. ', authors), collapse=', '))

  authors
}

get_institution <- function(slick) {
  institutions <- Institution(slick)
  if (length(institutions)>1)
    return(paste(paste0(1:length(institutions), '. ', institutions), collapse=', '))
  institutions
}

make_author_email <- function(Author, Email) {
  ind <- which(nchar(Email)>0)
  if (length(ind)>0) {
    Author[ind] <- paste0('<a href = "mailto: ', Email[ind], '">', Author[ind],'</a>')
  }
  Author
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
    mod_toplink_ui(ns(id)),
    uiOutput(ns('main'))
  )
}

#' Metadata Server Functions
#'
#' @noRd
mod_Metadata_server <- function(id, i18n, Slick_Object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home', metadatatab='Overview'))

    output$main <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboard::box(title=h3(strong(i18n$t('Overview'))),
                            width=12,
                            solidHeader=TRUE,
                            status = "primary",
                            uiOutput(ns('tabsetpanel'))
        )
      )
    })

    # main tabBox panels ----
    output$tabsetpanel <- renderUI({
      i18n <- i18n()
      tagList(
        uiOutput(ns('metadata')),
        uiOutput(ns('plotinfo'))
      )
    })

    # ---- metadata tab ----

    output$metadata <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      tagList(
        shinydashboard::box(width=6, status = "primary",
                            title=strong(Title(slick, i18n$get_translation_language())),
                            strong(Subtitle(slick, i18n$get_translation_language())),
                            p(strong(i18n$t('Created:')), Date(slick)),
                            p(strong(i18n$t('Author:')), get_authors(slick)),
                            p(strong(i18n$t('Institution:')), get_institution(slick)),
                            h4(strong(i18n$t('Summary'))),
                            shiny::markdown((Introduction(slick, i18n$get_translation_language())))

        )
      )
    })

    # ---- plotinfo tab ----

    output$plotinfo <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      tagList(
        shinydashboard::box(width=6,
                            status = "primary",
                            title=strong(i18n$t('About the Plots')),
                            p('This Slick file includes the following plots:'),
                            uiOutput(ns('plotlinks'))
                            )
      )
    })

    output$plotlinks <- renderUI({
      slick <- Slick_Object()
      i18n <- i18n()
      linklist <- list()

      quilt <- Quilt(slick)
      if (length(Value(quilt))>0) {
        info <- list(p(actionLink(ns('quilt'), 'Quilt: '),
                  i18n$t('A table of Performance Indicators for each Management Procedure')))
        linklist <- append(linklist, info)

        info <- list(p(actionLink(ns('tradeoff'), 'Trade-Off: '),
                  i18n$t('A scatter plot for two Performance Indicators included in the Quilt plot')))
        linklist <- append(linklist, info)
      }

      spider <- Spider(slick)
      if (length(Value(spider))>0) {
        info <- list(br())
        linklist <- append(linklist, info)

        info <- list(p(actionLink(ns('spider'), 'Spider: '),
                       i18n$t('SHORT DESCRIPTION')))
        linklist <- append(linklist, info)

        info <- list(p(actionLink(ns('zigzag'), 'Zigzag: '),
                       i18n$t('SHORT DESCRIPTION')))
        linklist <- append(linklist, info)
      }

      tagList(linklist)
    })


    observeEvent(input$quilt,{
      shinyjs::runjs("$('a[data-value=\"quilt\"]').tab('show');")
      shinyjs::delay(30,
                     shinyjs::runjs("$('a[data-value=\"Quilt\"]').tab('show');")
      )
    }, ignoreInit =TRUE)

    observeEvent(input$tradeoff,{
      shinyjs::runjs("$('a[data-value=\"quilt\"]').tab('show');")
      shinyjs::delay(30,
                     shinyjs::runjs("$('a[data-value=\"Trade-Off\"]').tab('show');")
      )
    }, ignoreInit =TRUE)


    observeEvent(input$spider,{
      shinyjs::runjs("$('a[data-value=\"spider\"]').tab('show');")
      shinyjs::delay(30,
                     shinyjs::runjs("$('a[data-value=\"h4>Spider</h4>\"]').tab('show');")
      )
    }, ignoreInit =TRUE)

    observeEvent(input$zigzag,{
      shinyjs::runjs("$('a[data-value=\"spider\"]').tab('show');")
      shinyjs::delay(30,
                     shinyjs::runjs("$('a[data-value=\"<h4>Zigzag</h4>\"]').tab('show');")
      )
    }, ignoreInit =TRUE)



  })
}

## To be copied in the UI
# mod_Metadata_ui("Metadata_1")

## To be copied in the server
# mod_Metadata_server("Metadata_1")

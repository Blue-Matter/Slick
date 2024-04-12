
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
    uiOutput(ns('page'))
  )
}

#' Metadata Server Functions
#'
#' @noRd
mod_Metadata_server <- function(id, i18n, Slick_Object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home', metadatatab='Overview'))


    output$page <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      tagList(
        shinydashboard::box(title=h3(strong(i18n$t('Overview'))),
                            width=12,
                            solidHeader=TRUE,
                            status = "primary",
                            tagList(
                              column(8, uiOutput(ns('metadata'))),
                              column(4, uiOutput(ns('plotinfo')))
                              )
        )
      )
    })

    # ---- metadata ----

    output$metadata <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      tagList(
        h3(Title(slick, i18n$get_translation_language())),
        strong(Subtitle(slick, i18n$get_translation_language())),
        p(strong(i18n$t('Created:')), Date(slick)),
        p(strong(i18n$t('Author:')), get_authors(slick)),
        p(strong(i18n$t('Institution:')), get_institution(slick)),
        h4(strong(i18n$t('Summary'))),
        shiny::markdown((Introduction(slick, i18n$get_translation_language())))


      )
    })

    # ---- plotinfo ----

    output$plotinfo <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      tagList(
        h3(i18n$t('About the Plots')),
        uiOutput(ns('plotlinks'))
      )
    })

    output$plotlinks <- renderUI({
      slick <- Slick_Object()
      i18n <- i18n()
      linklist <- list()

      if (length(Value(Boxplot(Slick_Object())))>0) {
        info <- list(p(actionLink(ns('boxplot'), 'Boxplot: ', icon("fa-regular fa-chart-candlestick")),
                       i18n$t("SHORT DESCRIPTION")
        ))
        linklist <- append(linklist, info)
      }

      if (length(Value(Kobe(Slick_Object())))>0) {
        info <- list(p(actionLink(ns('kobe'), 'Kobe: ', icon("table-cells-large")),
                       i18n$t("SHORT DESCRIPTION")
        ))
        linklist <- append(linklist, info)
      }

      if (length(Value(Quilt(Slick_Object())))>0) {
        info <- list(p(actionLink(ns('quilt'), 'Quilt: ', icon("table-cells")),
                  i18n$t('A table of Performance Indicators for each Management Procedure')))
        linklist <- append(linklist, info)
      }

      if (length(Value(Spider(Slick_Object())))>0) {
        info <- list(p(actionLink(ns('spider'), 'Spider: ', icon("fa-hexagon", class='fas')),
                       i18n$t("Also sometimes called a Radar chart, the Spider chart displays data across several dimensions")
        ))
        linklist <- append(linklist, info)
      }

      if (length(Value(Timeseries(Slick_Object())))>0) {
        info <- list(p(actionLink(ns('timeseries'), 'Timeseries: ', icon("chart-line-up-down")),
                       i18n$t("SHORT DESCRIPTION")
        ))
        linklist <- append(linklist, info)
      }

      if (length(Value(Tradeoff(Slick_Object())))>0) {
        info <- list(p(actionLink(ns('tradeoff'), 'Tradeoff: ', icon("chart-scatter")),
                       i18n$t('A scatter plot comparing two Performance Indicator')))
        linklist <- append(linklist, info)
      }
      tagList(linklist)
    })

    observeEvent(input$boxplot,{
      shinyjs::runjs("$('a[data-value=\"boxplot\"]').tab('show');")
    }, ignoreInit =TRUE)

    observeEvent(input$kobe,{
      shinyjs::runjs("$('a[data-value=\"kobe\"]').tab('show');")
    }, ignoreInit =TRUE)

    observeEvent(input$quilt,{
      shinyjs::runjs("$('a[data-value=\"quilt\"]').tab('show');")
    }, ignoreInit =TRUE)

    observeEvent(input$spider,{
      shinyjs::runjs("$('a[data-value=\"spider\"]').tab('show');")
    }, ignoreInit =TRUE)

    observeEvent(input$timeseries,{
      shinyjs::runjs("$('a[data-value=\"timeseries\"]').tab('show');")
    }, ignoreInit =TRUE)

    observeEvent(input$tradeoff,{
      shinyjs::runjs("$('a[data-value=\"tradeoff\"]').tab('show');")
    }, ignoreInit =TRUE)


  })
}

## To be copied in the UI
# mod_Metadata_ui("Metadata_1")

## To be copied in the server
# mod_Metadata_server("Metadata_1")

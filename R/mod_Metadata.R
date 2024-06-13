
get_authors <- function(slick) {
  authors <- Author(slick)
  if (length(authors)>1)
    return(paste(paste0(1:length(authors), '. ', authors), collapse=', '))

  authors
}

get_email <- function(slick) {
  email <- Email(slick)
  if (length(email)>1)
    return(paste(paste0(1:length(email), '. ', email), collapse=', '))

  email
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
        shiny::markdown(paste(i18n$t('**Email:**'), get_email(slick))),
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
        div(style='font-size: 16px;',
            uiOutput(ns('plotlinks'))
            )

      )
    })

    output$plotlinks <- renderUI({
      slick <- Slick_Object()
      i18n <- i18n()
      linklist <- list()

      if (!all(is.na(Value(Timeseries(slick))))) {
        info <- list(p(actionLink(ns('timeseries'), 'Time Series: ', icon("chart-line-up-down")),
                       i18n$t("Plots the time series of a user-specified variable (e.g., yield, biomass) during the historical period and the projection period for each management procedure.")
        ))
        linklist <- append(linklist, info)
      }

      if (!all(is.na(Value(Boxplot(slick))))) {
        info <- list(p(actionLink(ns('boxplot'), 'Boxplot: ', icon("fa-regular fa-chart-candlestick")),
                       i18n$t("A box plot, also called a box and whisker plot, displays the minimum, first quartile, median, third quartile, and maximum from a set of results. The boxplot tab includes an option to display the data as a violin plot, which is similar to a boxplot but also shows the density of data at different values.")
        ))
        linklist <- append(linklist, info)
      }

      if (!all(is.na(Value(Kobe(slick))))) {
        info <- list(p(actionLink(ns('kobe'), 'Kobe: ', icon("table-cells-large")),
                       i18n$t("A trade-off plot comparing the performance of MPs with respect to biomass (on the x-axis) and fishing mortality (on the y-axis).  The Kobe tab includes an option to display the data as a Kobe time plot, which shows the percentage of runs that fall in each of the Kobe quadrants in each projection year.")
        ))
        linklist <- append(linklist, info)
      }

      if (!all(is.na(Value(Quilt(slick))))) {
        info <- list(p(actionLink(ns('quilt'), 'Quilt: ', icon("table-cells")),
                  i18n$t('A table of performance indicators for each management procedure. Darker shading indicates better performance.')))
        linklist <- append(linklist, info)
      }

      if  (!all(is.na(Value(Spider(slick))))) {
        info <- list(p(actionLink(ns('spider'), 'Spider: ', icon("fa-hexagon", class='fas')),
                       i18n$t("Also sometimes referred to as Radar charts or web diagrams, these plots show results for three or more performance indicators, each represented on an axis starting from the same center point.")
        ))
        linklist <- append(linklist, info)
      }


      if (!all(is.na(Value(Tradeoff(slick))))) {
        info <- list(p(actionLink(ns('tradeoff'), 'Tradeoff: ', icon("chart-scatter")),
                       i18n$t('A scatter plot comparing two performance indicators.')))
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

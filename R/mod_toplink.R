#' toplink UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_toplink_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('topmenu'))

  )
}

#' toplink Server Functions
#'
#' @noRd
mod_toplink_server <- function(id, links){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    make_link <- function(name, link) {
      actionLink(ns(name), link)
    }

    output$topmenu <- renderUI({
      link_list <- list()
      cnt <- 1
      for (i in 1:length(links)) {
        link_list[[cnt]] <- make_link(names(links)[i],links[[i]])
        if (i<length(links)) {
          cnt <- cnt + 1
          link_list[[cnt]] <- '>'
        }
        cnt <- cnt + 1
      }

      tagList(link_list)
    })

    observeEvent(input[[names(links)[1]]],{
      tt <- paste0("$('a[data-value=", '\"', names(links)[1], "\"]').tab('show');")
      shinyjs::runjs(tt)
    }, ignoreInit =TRUE)

    observeEvent(input[[names(links)[2]]],{
      tt <- paste0("$('a[data-value=", '\"', names(links)[2], "\"]').tab('show');")
      shinyjs::runjs(tt)
    }, ignoreInit =TRUE)

    observeEvent(input[[names(links)[3]]],{
      tt <- paste0("$('a[data-value=", '\"', names(links)[3], "\"]').tab('show');")
      shinyjs::runjs(tt)
    }, ignoreInit =TRUE)

    observeEvent(input[[names(links)[4]]],{
      tt <- paste0("$('a[data-value=", '\"', names(links)[4], "\"]').tab('show');")
      shinyjs::runjs(tt)
    }, ignoreInit =TRUE)

    observeEvent(input[[names(links)[5]]],{
      tt <- paste0("$('a[data-value=", '\"', names(links)[5], "\"]').tab('show');")
      shinyjs::runjs(tt)
    }, ignoreInit =TRUE)

  })
}

## To be copied in the UI
# mod_toplink_ui("toplink_1")

## To be copied in the server
# mod_toplink_server("toplink_1")

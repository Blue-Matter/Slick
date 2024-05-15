#' Resources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Resources_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             h3('Slick Developer Manual'),
             p(a(href='https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html',
                 'Developer Guide',
                 target="_blank")),
             h3('MSE-Related Resources'),
             htmlOutput(ns('content'))
      )
    )
  )
}

#' Resources Server Functions
#'
#' @noRd
mod_Resources_server <- function(id, i18n){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Resources <- Slick::Resources
    Footnotes <- Slick::Footnotes

    output$content <-  renderUI({
      headings <- Resources$Heading |> unique()

      output_list <- lapply(seq_along(headings), function(x) {
        heading <- headings[x]
        subDat <- Resources |> dplyr::filter(Heading==heading)
        txt <- list()
        if (heading == 'Media') {
          for (i in 1:nrow(subDat)) {
            text <- strsplit(subDat$Title[i], " ")[[1]]
            title <- text[1]
            desc <- paste(text[-1], collapse=" ")
            txt[[i]] <-  p(paste0(title, ':'), a(href=subDat$Link[i], desc, target="_blank"))
          }
        } else {
          for (i in 1:nrow(subDat)) {
            foot <- subDat$Footnote[i]
            if (is.na(foot)) foot <-''
            txt[[i]] <- p(HTML(paste0(a(href=subDat$Link[i], subDat$Title[i], target="_blank"),
                                      tags$sup(foot))),
                          paste0("(",subDat$Reference[i], ")"))
          }
        }

        tagList(
          h4(toupper(heading)),
          txt,
          br()
        )
      })

      footnote_list <- lapply(Footnotes$Number, function(x) {
        tagList(
          p(tags$sup(Footnotes$Number[x]), Footnotes$Footnote[x])
        )
      })

      tagList(
        do.call(tagList, output_list),
        do.call(tagList, footnote_list)
      )
    })
  })

}

## To be copied in the UI
# mod_Resources_ui("Resources_1")

## To be copied in the server
# mod_Resources_server("Resources_1")

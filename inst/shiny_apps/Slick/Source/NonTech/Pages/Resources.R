ResourcesServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {

                 output$content <-  renderUI({
                   Data <- readxl::read_xlsx('./data/Resources.xlsx')
                   Footnotes <- readxl::read_xlsx('./data/Resources.xlsx', sheet="Footnotes")
                   headings <- Data$Heading %>% unique()

                   output_list <- lapply(seq_along(headings), function(x) {
                     heading <- headings[x]
                     subDat <- Data %>% dplyr::filter(Heading==heading)
                     txt <- list()
                     if (heading == 'Media') {
                       for (i in 1:nrow(subDat)) {
                         text <- strsplit(subDat$Title[i], " ")[[1]]
                         title <- text[1]
                         desc <- paste(text[-1], collapse=" ")
                         txt[[i]] <-  p(title, a(href=subDat$Link[i], desc, target="_blank"))
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


ResourcesUI <- function(id, label="resources") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             h3('MANUALS'),
             shiny::actionButton(inputId='ab1', label="User Guide",
                                 icon = icon("question-circle"),
                                 onclick ="window.open('https://blue-matter.github.io/openMSE/SLICK-User-Guide.html', '_blank')"),
             shiny::actionButton(inputId='ab1', label="Developers' Manual",
                                 icon = icon("info-circle"),
                                 onclick ="window.open('https://blue-matter.github.io/openMSE/SLICK-Developer-Guide.html', '_blank')"),

             br(),
             h3('MSE-Related Resources'),

             htmlOutput(ns('content'))
      )
    )
  )
}


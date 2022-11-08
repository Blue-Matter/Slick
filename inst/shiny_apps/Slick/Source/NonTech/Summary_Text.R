

summaryServer <- function(id, Det, MPkeep, Detkeep, SNkeep, Object, summaryFun, minPMs=3, ...) {

  moduleServer(id, function(input, output, session) {

    Summary <- reactiveValues(edit=TRUE,
                              save=FALSE,
                              text='',
                              text2='')

    observeEvent(input$edit_summary, {
      Summary$edit <- FALSE
      Summary$save <- TRUE
    })

    observeEvent(input$save_summary, {
      Summary$edit <- TRUE
      Summary$save <- FALSE
    })


    output$summarytext <- renderUI({
      out <- HTML(paste0("<pre>", Summary$text(),"</pre>"))
      out
    })

    Summary$text <- reactive({
      if(!Object$Loaded) return()

      if (is.null(input$summary)) {

        nSN <- nrow(Object$obj$OM$Design)
        nSNs <- sum(SNkeep$selected)

        nPMd <- dim(Object$obj$Perf$Det$Values)[3]
        nPMds <- sum(Detkeep$selected)

        nMP <- length(Object$obj$MP$Labels)
        nMPs <- sum(MPkeep$selected)


        if (nMPs>0 & nPMds >=minPMs & nSNs>0) {
          Summary$text2 <- summaryFun(Det, MPkeep, Detkeep, SNkeep, Object, ...)
        } else {
          if (nPMds <minPMs)
            Summary$text2 <- paste0('Insufficient number of Performance Metrics (must select at least ', minPMs, ")")
        }


      }
      if (!is.null(input$summary)) {
        if (input$save_summary>0)
          Summary$text2 <- input$summary
      }
      Summary$text2
    })
    #

    output$edit_ <- reactive({Summary$edit})
    outputOptions(output, "edit_", suspendWhenHidden = FALSE)

    output$save_ <- reactive({Summary$save})
    outputOptions(output, "save_", suspendWhenHidden = FALSE)

    output$summary <- renderUI({
      if(!Object$Loaded) return()

      if (Summary$edit>0) {
        tagList(
          div(
            h4('Summary'),
            htmlOutput(session$ns("summarytext")),
            div(style="display:inline-block",
              actionButton(session$ns("edit_summary"), "Edit Summary",
                           icon = icon("edit", verify_fa =FALSE)),
              style="float:left"
            )
            # div(style="display:inline-block",
            #   actionButton(session$ns("gen_summary"), "Auto Generate",
            #                icon = icon("redo")),
            #   style="float:right"
            # )
          )
        )
      } else {
        tagList(
          textAreaInput(session$ns("summary"), label = h4("Summary"),
                        value = Summary$text2,
                        resize='both', height="300px")  %>%
            shiny::tagAppendAttributes(style = 'width: 100%; height: 100%;'),
          div(
            actionButton(session$ns("save_summary"), "Save Summary",
                         icon = icon("save", verify_fa =FALSE))
          )
        )
      }



      # tagList(
      #   conditionalPanel(condition='output.edit_>0', ns=NS(id),
      #                    div(
      #                      h4('Summary'),
      #                      htmlOutput(session$ns("summarytext")),
      #                      div(
      #                        actionButton(session$ns("edit_summary"), "Edit Summary",
      #                                     icon = icon("edit"))
      #                      )
      #                    )
      #   ),
      #
      #   conditionalPanel(condition="output.save_>0", ns=NS(id),
      #                    textAreaInput(session$ns("summary"), label = h4("Summary"),
      #                                  value = Summary$text(),
      #                                  resize='both')  %>%
      #                      shiny::tagAppendAttributes(style = 'width: 100%; height: 100%;'),
      #                    div(
      #                      actionButton(session$ns("save_summary"), "Save Summary",
      #                                   icon = icon("save"))
      #                    )
      #   )
      #
      #
      # )
    })

  })
}

summaryUI <- function(id) {

  ns <- NS(id)
  tagList(
    uiOutput(ns('summary'), class='page_summary')
  )
}

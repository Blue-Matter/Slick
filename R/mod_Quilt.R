#' Quilt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Quilt_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('page'))

  )
}

#' Quilt Server Functions
#'
#' @noRd
mod_Quilt_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Quilt'))),
                                shiny::tabsetPanel(id='quilt_tabs',
                                                   shiny::tabPanel(i18n$t('Quilt'),
                                                                   br(),
                                                                   shinydashboard::box(width=12,
                                                                                       status='primary',
                                                                                       uiOutput(ns('quilt'))
                                                                   )
                                                   ),
                                                   shiny::tabPanel(i18n$t('Trade-Off'),
                                                                   uiOutput(ns('tradeoff'))
                                                   )

                                ),
                                sidebar = shinydashboardPlus::boxSidebar(id='quiltsidebar',
                                                                         column(12, align = 'left', class='multicol',
                                                                                uiOutput(ns('filters'))
                                                                                )

                                )

        )
      )
    })

    output$filters <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      quilt <- Quilt(slick)
      mps <- MPs(slick)

      Value(quilt)

      tagList(
        tabsetPanel(
          tabPanel(i18n$t('Operating Models'),
                   br(),
                   uiOutput(ns('OM_defaults')),
                   uiOutput(ns('OM_filters'))
          ),
          tabPanel(i18n$t('Management Procedures'),
                   p('test')
          ),
          tabPanel(i18n$t('Performance Indicators'),
                   p('tst')
          )
        )


      )

    })

    selectedOMs <- function(i, OM) {
      defaults <- Default(OM)
      if (length(defaults)<1) {
        out <- 1:length(Label(OM)[[i]])
      } else {
        if (length(defaults)<i) {
          out <- NULL
        } else {
          out <- defaults[[i]]
        }
      }
      out
    }


    output$OM_defaults <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      oms <- OMs(slick)
      defaults <<- Default(oms)

      stop('fix the Defaults slot in slick2SlickData')

      if (length(defaults)>0) {
        tagList(
          actionButton(ns('reset_OMs'), i18n$t('Reset Defaults'))
        )
      }
    })

    observeEvent(input$reset_OMs, {
      slick <- Slick_Object()
      oms <- OMs(slick)
      defaults <- Default(oms)
      factors <- colnames(oms@Design)
      for(i in 1:length(factors)) {
        updateCheckboxGroupInput(inputId=paste0("Filter_OM",i),
                                 selected=selectedOMs(i, oms))
      }
    })

    output$OM_filters <- renderUI({
      i18n <- i18n()
      oms <- OMs(Slick_Object())
      labels <- Label(oms, i18n$get_translation_language())
      factors <- colnames(oms@Design)

      tagList(
        lapply(1:length(factors), function(i) {
          checkboxGroupInput(ns(paste0("Filter_OM",i)),
                             label=factors[i],
                             selected=selectedOMs(i, oms),
                             inline=TRUE,
                             choiceNames=labels[[i]],
                             choiceValues=1:length(labels[[i]])
                             )
        })
      )
    })

    output$quilt <- renderUI({
      i18n <- i18n()

      tagList(
        h3('Heading'),
        p('test paragraph')


      )
    })

    output$tradeoff <- renderUI({

    })


  })
}

## To be copied in the UI
# mod_Quilt_ui("Quilt")

## To be copied in the server
# mod_Quilt_server("Quilt")

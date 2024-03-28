colorRampAlpha <- function(..., n, alpha) {
  colors <- grDevices::colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}



make_Quilt <- function(quilt, mp_label) {
  # quilt <<- quilt
  Values <- Value(quilt) |>
    apply(2:3, median) |>
    signif(3)
  if (all(is.na(Values))) {
    return(NULL)
  }
  rownames(Values) <- mp_label
  pm_labels <- Label(quilt)
  colnames(Values) <- pm_labels
  cols <- Color(quilt)


  outable <-  DT::datatable(Values, extensions = 'Buttons',
                          options = list(dom = 'tB',
                                         pageLength =100,
                                         buttons=c('copy', 'csv'),
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                         scrollX = TRUE
                                         ),
                          filter = list(
                            position = 'top', clear = FALSE
                          ), selection = 'none')

  for (i in 1:ncol(Values)) {
    pm <- pm_labels[i]
    val_range <- range(Values[,i])
    cuts <- quantile(Values[,i], seq(0, 1, by=0.1)) |>
      as.numeric()
    values <- rev(colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5) )

    outable <- outable |>
      DT::formatStyle(
        pm,
        backgroundColor = DT::styleInterval(cuts=cuts,
                                            values=values)
      )
  }
  outable

}


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
#' @importFrom dplyr %>%
#' @noRd
mod_Quilt_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Filter_Selected <- mod_Filter_server('quilt', i18n, Slick_Object)

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Quilt and Trade-Off'))),
                                shiny::tabsetPanel(id='quilt_tabs',
                                                   shiny::tabPanel(i18n$t('Quilt'),
                                                                   br(),
                                                                   uiOutput(ns('quilt'))
                                                   ),
                                                   shiny::tabPanel(i18n$t('Trade-Off'),
                                                                   uiOutput(ns('tradeoff'))
                                                   )

                                ),
                                sidebar = shinydashboardPlus::boxSidebar(id='quiltsidebar',
                                                                         column(12, align = 'left', class='multicol',
                                                                                mod_Filter_ui(ns('quilt'))
                                                                                )
                                )
        ) %>% {
          htmltools::tagQuery(.)$
            find("#quiltsidebar")$
            removeAttrs("data-original-title")$
            addAttrs(`data-original-title`="Filters")$
            allTags()
        }
      )
    })

    filtered_quilt <- reactive({
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      quilt <- Quilt(slick)
      # filter OMs
      if (!is.null(selected_OMs)) {
        Value(quilt) <- Value(quilt)[selected_OMs,,, drop=FALSE]
      }
      # filter MPs

      # filter PMs

      quilt
    })

    nOM <- reactive({
      dim(Value(filtered_quilt()))[1]
    })

    MP_labels <- reactive({

    })

    nMP <- reactive({
      length(MP_labels())
    })

    output$quilt <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      quilt <- filtered_quilt()

      Values <- Value(quilt)
      nOMs <- dim(Values)[1]

      MPs <- MPs(slick)
      mp_label <- Label(MPs(slick))
      # add MP Filter here
      nMPs <- length(mp_label)

      PMs <- Label(quilt)
      nPMs <- length(PMs)

      tagList(
        shinydashboard::box(width=4,
                            h4(strong(i18n$t("READING THIS CHART"))),
                            htmlOutput(ns('reading_quilt'))
        ),
        shinydashboard::box(width=8,
                            status='primary',
                            title=paste(nMP(), i18n$t('Management Procedures. Median values over'), nOM(), i18n$t('Operating Models')),
                            make_Quilt(quilt, mp_label)
        )
      )
    })

    output$reading_quilt <- renderUI({

      tagList(
        # p('This chart', strong('compares the performance of ', nMP,
        #                        ' management procedures (MP) against ', nPM,
        #                        ' performance metrics.'))
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

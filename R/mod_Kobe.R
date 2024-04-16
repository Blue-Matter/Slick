#' Kobe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Kobe_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))
  )
}

#' Kobe Server Functions
#'
#' @noRd
mod_Kobe_server <- function(id, i18n, Slick_Object, window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      boxplot='Boxplot'))

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Boxplot',
                                        parent_session=session)

    mod_subtitle_server(id, i18n, nOM, nMP)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)
    mod_Report_Add_server("Report_Add_2", i18n, parent_session=session, Report, plot_object)

    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Kobe'))),
                                br(),
                                column(12, mod_subtitle_ui(ns(id))),
                                column(5,
                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns("plotselect"),
                                         choiceNames = c(i18n$t('Overall'),
                                                         i18n$t('Kobe Time')
                                         ),
                                         choiceValues=c('overall',  'kobetime')
                                       )
                                ),
                                column(2),
                                column(5,
                                       htmlOutput(ns('PM_dropdown')),

                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns('plottype'),
                                         choiceNames  = c(i18n$t('Boxplot'),
                                                          i18n$t('Violin'),
                                                          i18n$t('Both')),
                                         choiceValues = c('boxplot', 'violin', 'both'),
                                         checkIcon = list(
                                           yes = tags$i(class = "fa fa-check-square",
                                                        style = "color: steelblue"),
                                           no = tags$i(class = "fa fa-square-o",
                                                       style = "color: steelblue"))

                                       )
                                ),
                                column(12,
                                       conditionalPanel("input.plotselect=='overall'", ns=ns,
                                                        mod_Boxplot_overall_ui(ns("Boxplot_overall_1"))
                                       ),
                                       conditionalPanel("input.plotselect=='byom'", ns=ns,
                                                        mod_Boxplot_OM_ui(ns("Boxplot_OM_1"))
                                       )
                                ),
                                sidebar = shinydashboardPlus::boxSidebar(id=ns('filtersidebar'),
                                                                         icon=icon('fa-xl fa-filter', class='fa-regular'),
                                                                         column(12, align = 'left', class='multicol',
                                                                                mod_Filter_ui(ns(id))
                                                                         )
                                )
        )
      )
    })


    output$PM_dropdown <- renderUI({
      i18n <- i18n()


      sel1 <- Codes[grepl('BMSY', Codes)][1][[1]]
      sel2 <- Codes[grepl('FMSY', Codes)][1][[1]]
      if (is.na(sel1)|| is.null(sel1)) sel1 <- Codes[1][[1]]
      if (is.na(sel2) || is.null(sel2)) sel2 <- Codes[2][[1]]

      tagList(
        h4('Select Performance Metrics'),
        selectInput(session$ns('selectPM1'),
                    'Horizontal axis',
                    choices=Codes,
                    selected=sel1),
        selectInput(session$ns('selectPM2'),
                    'Vertical axis',
                    choices=Codes,
                    selected=sel2
        )
      )
    })


  })
}

## To be copied in the UI
# mod_Kobe_ui("Kobe_1")

## To be copied in the server
# mod_Kobe_server("Kobe_1")

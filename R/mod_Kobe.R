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

    mod_subtitle_server(id, i18n, nOM, nMP)

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      kobe='Kobe'))

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Kobe',
                                        parent_session=session,
                                        incPM=FALSE)

    mod_Kobe_overall_server("Kobe_overall_1", i18n, filtered_slick,
                            plottype,
                            nOM, nMP, nPM, parent_session=session,
                            window_dims)

    mod_Kobe_time_server("Kobe_time_1", i18n, filtered_slick,
                         plottype,
                         nOM, nMP, nPM, parent_session=session,
                         window_dims)

    # button_pushed <- mod_Report_Add_Button_server("report_button", i18n)
    # mod_Report_Add_server("Report_Add_2", i18n, parent_session=session, Report, plot_object)

    filtered_slick <- reactive({
      if (is.null(Slick_Object())) return(NULL)
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      selected_MPs <- Filter_Selected$MPs

      kobe <- Kobe(slick)

      dd <- dim(Value(kobe))
      if (length(selected_OMs)==dd[2]) {
        # filter OMs
        if (!is.null(selected_OMs)) {
          Value(kobe) <- Value(kobe)[,selected_OMs,,,, drop=FALSE]

        }
        # filter MPs
        if (!is.null(selected_MPs)) {
          Value(kobe) <- Value(kobe)[,,selected_MPs,,, drop=FALSE]
          metadata <- Metadata(MPs(slick))
          Metadata(MPs(slick)) <- metadata[selected_MPs,]
        }

      }
      Kobe(slick) <- kobe
      slick
    })

    dims <- reactive({
      d <- filtered_slick() |>
        Kobe() |>
        Value() |>
        dim()
    })

    nOM <- reactive({
      dims()[2]
    })

    nPM <- reactive({
      dims()[4]
    })

    nMP <- reactive({
      filtered_slick() |>
        MPs() |>
        Metadata() |>
        nrow()
    })


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
                                column(12,
                                       conditionalPanel("input.plotselect=='overall'", ns=ns,
                                                        mod_Kobe_overall_ui(ns("Kobe_overall_1"))
                                       ),
                                       conditionalPanel("input.plotselect=='kobetime'", ns=ns,
                                                        mod_Kobe_time_ui(ns("Kobe_time_1"))
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
  })
}

## To be copied in the UI
# mod_Kobe_ui("Kobe_1")

## To be copied in the server
# mod_Kobe_server("Kobe_1")

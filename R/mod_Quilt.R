

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
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))
  )
}

#' Quilt Server Functions
#' @noRd
mod_Quilt_server <- function(id, i18n, Slick_Object, window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      quilt='Quilt'))

    mod_subtitle_server('quiltsubtitle', i18n, nOM, nMP, nPM)

    Filter_Selected <- mod_Page_Filter_server("quiltfilter",i18n, Slick_Object,
                                              slot='Quilt', minPM=2, FALSE)

    filtered_slick <- reactive({
      FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  as.numeric(Filter_Selected$PMs),
                  'Quilt')
    })

    dims <- reactive({
      filtered_slick() |> Quilt() |> Value() |> dim()
    })

    nOM <- reactive({
      dims()[1]
    })

    MP_labels <- reactive({
      metadata <- Metadata(MPs(Slick_Object()))
      metadata$Code[Filter_Selected$MPs]
    })

    nMP <- reactive({
      length(MP_labels())
    })

    nPM <- reactive({
      dims()[3]
    })


    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Quilt'))),
                                column(12, align = 'left', class='multicol',
                                       tagList(
                                         br(),
                                         column(12, mod_subtitle_ui(ns('quiltsubtitle'))
                                         ),
                                         column(3,
                                                h4(strong(i18n$t("Reading this Chart"))),
                                                htmlOutput(ns('reading'))
                                         ),
                                         column(9,
                                                # mod_Report_Add_Button_ui(ns('report_button')),
                                                br(),
                                                uiOutput(ns('quilttable'))
                                         )
                                       )
                                )
        )
      )
    })

    output$quilttable <- renderUI({
      req(filtered_slick)
      plot_object() |> flextable::autofit() |> flextable::htmltools_value()
    })

    # observeEvent(button_pushed(), {
    #   shiny::showModal(mod_Report_Add_ui(ns("Report_Add_1")))
    # })

    plot_object <- reactive({
      req(filtered_slick)
      plotQuilt(filtered_slick(), MP_labels(), i18n()$get_translation_language(),
                kable=TRUE)
    })

    output$reading <- renderUI({
      i18n <- i18n()
      tagList(
        p(i18n$t('This table ...')),

        p(i18n$t('Use the'), actionLink(ns('openfilter'), i18n$t('Filter'), icon=icon('fa-lg fa-filter', class='fa-regular')),
          i18n$t('button to filter the Management Procedures, Operating Models, and Performance Indicators.')
        )
      )
    })

    nOM <- reactive({
      dd <- filtered_slick() |> Quilt() |> Value() |> dim()
      dd[1]
    })

    MP_labels <- reactive({
      metadata <- Metadata(MPs(Slick_Object()))
      metadata$Code[Filter_Selected$MPs]
    })

    nMP <- reactive({
      length(MP_labels())
    })

  })
}

## To be copied in the UI
# mod_Quilt_ui("Quilt")

## To be copied in the server
# mod_Quilt_server("Quilt")

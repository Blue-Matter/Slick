#' Boxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_Boxplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_toplink_ui(ns(id)),
    uiOutput(ns('page'))
  )
}

#' Boxplot Server Functions
#'
#' @noRd
mod_Boxplot_server <- function(id, i18n, Slick_Object, window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_toplink_server(id, links=list(hometab='Home',
                                      metadatatab='Overview',
                                      boxplot='Boxplot'))

    Filter_Selected<- mod_Filter_server(id, i18n, Slick_Object,
                                        slot='Boxplot',
                                        parent_session=session)

    mod_Boxplot_overall_server("Boxplot_overall_1",
                               i18n, filtered_slick, plottype,
                               nOM, nMP, nPM, parent_session,
                               window_dims)

    mod_Boxplot_OM_server("Boxplot_OM_1", i18n, filtered_slick, plottype,
                          nOM, nMP, nPM, parent_session,
                          window_dims)

    mod_subtitle_server(id, i18n, nOM, nMP)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)
    mod_Report_Add_server("Report_Add_2", i18n, parent_session=session, Report, plot_object)


    output$page <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboardPlus::box(width=12,
                                status='primary',
                                solidHeader=TRUE,
                                title=h3(strong(i18n$t('Boxplot'))),
                                br(),
                                column(12, mod_subtitle_ui(ns(id))),
                                column(5,
                                       shinyWidgets::radioGroupButtons(
                                         inputId = ns("plotselect"),
                                         choiceNames = c(i18n$t('Overall'),
                                                         i18n$t('By Operating Model')
                                         ),
                                         choiceValues=c('overall',  'byom')
                                       )
                                ),
                                column(2),
                                column(5,
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

    plottype <- reactive({
      match(input$plottype, c('boxplot', 'violin', 'both'))
    })

    filtered_slick <- reactive({
      if (is.null(Slick_Object())) return(NULL)
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      selected_MPs <- Filter_Selected$MPs
      selected_PMs <- Filter_Selected$PMs

      boxplot <- Boxplot(slick)

      dd <- dim(Value(boxplot))
      if (length(selected_OMs)==dd[2]) {
        # filter OMs
        if (!is.null(selected_OMs)) {
          Value(boxplot) <- Value(boxplot)[,selected_OMs,,, drop=FALSE]

        }
        # filter MPs
        if (!is.null(selected_MPs)) {
          Value(boxplot) <- Value(boxplot)[,,selected_MPs,, drop=FALSE]
          metadata <- Metadata(MPs(slick))
          Metadata(MPs(slick)) <- metadata[selected_MPs,]
        }
        # filter PMs
        if (!is.null(selected_PMs)) {
          Metadata(boxplot) <- Metadata(boxplot)[selected_PMs, ]
          Value(boxplot) <- Value(boxplot)[,,,selected_PMs, drop=FALSE]
        }

      }
      Boxplot(slick) <- boxplot
      slick
    })

    dims <- reactive({
      d <- filtered_slick() |>
        Boxplot() |>
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



  })
}

## To be copied in the UI
# mod_Boxplot_ui("Boxplot_1")

## To be copied in the server
# mod_Boxplot_server("Boxplot_1")

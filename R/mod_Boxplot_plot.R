#' Boxplot_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Boxplot_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot'))
  )
}

#' Boxplot_plot Server Functions
#'
#' @noRd
mod_Boxplot_plot_server <- function(id, i18n,
                                    Slick_Object,
                                    Filter_Selected,
                                    parent_session=session,
                                    window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)
    mod_Report_Add_server("Report_Add_2", i18n, parent_session=session, Report, plot_object)


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
        Spider() |>
        Value() |>
        dim()
    })

    nOM <- reactive({
      dims()[1]
    })

    nPM <- reactive({
      dims()[3]
    })

    nMP <- reactive({
      filtered_slick() |>
        MPs() |>
        Metadata() |>
        nrow()
    })


    mod_subtitle_server(id, i18n, nOM, nMP)


    mod_Boxplot_overall_server("Boxplot_overall_1",
                              i18n, filtered_slick,
                              nOM, nMP, nPM, parent_session,
                              window_dims)

    mod_Boxplot_OM_server("Boxplot_OM_1", i18n, filtered_slick,
                         nOM, nMP, nPM, parent_session)



    output$plot <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()

      tagList(
        br(),
        column(12, mod_subtitle_ui(ns(id))
        ),
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
                 choices = c(i18n$t('Boxplot'),
                             i18n$t('Violin'),
                             i18n$t('Both')),
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
        )
      )
    })


  })
}

## To be copied in the UI
# mod_Boxplot_plot_ui("Boxplot_plot_1")

## To be copied in the server
# mod_Boxplot_plot_server("Boxplot_plot_1")

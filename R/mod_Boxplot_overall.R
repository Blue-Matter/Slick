#' Boxplot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Boxplot_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_Report_Add_Button_ui(ns('report_button')),
    uiOutput(ns('results'))
  )
}

#' Boxplot_overall Server Functions
#'
#' @noRd
mod_Boxplot_overall_server <- function(id, i18n, filtered_slick,
                                       plottype,
                                       nOM, nMP, nPM, parent_session,
                                       window_dims, Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Boxplot',
                          window_dims)

    button_pushed <- mod_Report_Add_Button_server("report_button", i18n)

    observeEvent(button_pushed(), {

      p_type <- switch(plottype(),
                       '1'='boxplot',
                       '2'='violin',
                       '3'='both'
      )
      Plot_Object(plotBoxplot(filtered_slick(),
                              1:nPM(),
                              p_type))

      if(!inherits(Plot_Object(), 'NULL'))
        shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    })


    output$boxplot_plot <- renderPlot({
      plotBoxplot(filtered_slick(),
                  1:nPM(),
                  type='boxplot')
    })

    output$boxplots <- renderUI({
      tagList(
        loading_spinner(
          plotOutput(ns('boxplot_plot'), width=plot_width(), height=plot_height())
        )
      )
    })

    output$violin_plot <- renderPlot({
      plotBoxplot(filtered_slick(),
                  1:nPM(),
                  type='violin')
    })

    output$violins <- renderUI({
      tagList(
        loading_spinner(
          plotOutput(ns('violin_plot'), width=plot_width(), height=plot_height())
        )
      )
    })

    output$both_plot <- renderPlot({
      plotBoxplot(filtered_slick(),
                  1:nPM(),
                  type='both')
    })

    output$both <- renderUI({
      tagList(
        loading_spinner(
          plotOutput(ns('both_plot'), width=plot_width(), height=plot_height())
        )
      )
    })


    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.6
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    # plot_width_text <- reactive({
    #   paste0('width: ', plot_width(), '; height: 320px;')
    #
    # })

    plot_height_calc <- reactive({
      dd <- window_dims()
      val <- dd[2] * 0.6
      paste0(val, 'px')
    })

    plot_height <- plot_height_calc |> debounce(500)


    output$selectedtype <- reactive({
      plottype()
    })

    outputOptions(output, "selectedtype", suspendWhenHidden = FALSE)

    output$results <- renderUI({
      tagList(
        conditionalPanel("output.selectedtype=='1'", ns=ns,
                        uiOutput(ns('boxplots'))
        ),
        conditionalPanel("output.selectedtype=='2'", ns=ns,
                         uiOutput(ns('violins'))
        ),
        conditionalPanel("output.selectedtype=='3'", ns=ns,
                         uiOutput(ns('both'))
        )
      )
    })
  })
}





## To be copied in the UI
# mod_Boxplot_overall_ui("Boxplot_overall_1")

## To be copied in the server
# mod_Boxplot_overall_server("Boxplot_overall_1")

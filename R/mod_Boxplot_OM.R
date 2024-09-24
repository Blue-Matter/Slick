#' Boxplot_OM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Boxplot_OM_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('results'))
  )
}

#' Boxplot_OM Server Functions
#'
#' @noRd
mod_Boxplot_OM_server <- function(id, i18n, filtered_slick,
                                  plottype,
                                  nOM, nMP, nPM, parent_session,
                                  window_dims, Report,
                                  selected_oms){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()

    mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
                          Report,
                          Plot_Object=Plot_Object, 'Boxplot',
                          window_dims)



    button_boxplot <- reactiveValues()
    button_violin <- reactiveValues()
    button_both <- reactiveValues()

    observeEvent(filtered_slick(), {

      if (!is.null(filtered_slick())) {
        n <- nPM()
        if (!is.na(n) && length(n)>0 && n>0) {
          for (i in 1:n) {
            nm <- paste0('report_button_boxplot_', i)
            button_boxplot[[nm]] <- mod_Report_Add_Button_server(nm, i18n)
          }
        }
      }
    })

    observeEvent(filtered_slick(), {
      if (!is.null(filtered_slick())) {
        n <- nPM()
        if (!is.na(n) && length(n)>0 && n>0) {
          for (i in 1:n) {
            nm <- paste0('report_button_violin_', i)
            button_violin[[nm]] <- mod_Report_Add_Button_server(nm, i18n)
          }
        }
      }
    })

    observeEvent(filtered_slick(), {
      if (!is.null(filtered_slick())) {
        n <- nPM()
        if (!is.na(n) && length(n)>0 && n>0) {
          for (i in 1:n) {
            nm <- paste0('report_button_both_', i)
            button_both[[nm]] <- mod_Report_Add_Button_server(nm, i18n)
          }
        }
      }
    })

    observe({
      if (!is.null(filtered_slick())) {
        n <- nPM()
        if (!is.na(n) && length(n)>0 && n>0) {
          lapply(1:n, function(i) {

            nm1 <- paste0('report_button_boxplot_', i)
            observeEvent(button_boxplot[[nm1]](), {

              Plot_Object(plotBoxplot(filtered_slick(), i, 'boxplot', byOM=TRUE))
              if(!inherits(Plot_Object(), 'NULL'))
                shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
            }, ignoreInit = TRUE)

            nm2 <- paste0('report_button_violin_', i)
            observeEvent(button_violin[[nm2]](), {

              Plot_Object(plotBoxplot(filtered_slick(), i, 'violin', byOM=TRUE))
              if(!inherits(Plot_Object(), 'NULL'))
                shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
            }, ignoreInit = TRUE)
            nm3 <- paste0('report_button_both_', i)
            observeEvent(button_both[[nm3]](), {
              Plot_Object(plotBoxplot(filtered_slick(), i, 'both', byOM=TRUE))
              if(!inherits(Plot_Object(), 'NULL'))
                shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
            }, ignoreInit = TRUE)
          })
        }
      }
    })


    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.6
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    plot_height_calc <- reactive({
      if (!is.null(filtered_slick())) {
        nom <- nOM()
        return(max(ceiling(nom/4) * 250, 250))
      } else {
        return(400)
      }
    })

    plot_height <- plot_height_calc |> debounce(500)

    plot_height_text <- reactive({
      paste0(plot_height(), 'px')
    })

    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.6
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    plot_width_text <- reactive({
      height <- paste0('height: ', plot_height()+120, 'px')
      paste0('width: ', plot_width(), '; ', height)
    })


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


    output$boxplots <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("boxplot", mm, sep="")
          tagList(loading_spinner(plotOutput(session$ns(plotname),
                                             width=plot_width(),
                                             height=plot_height())),
                  mod_Report_Add_Button_ui(ns(paste0('report_button_boxplot_', mm)))
                  )
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    output$violins <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("violin", mm, sep="")
          tagList(loading_spinner(plotOutput(session$ns(plotname),
                                             width=plot_width(),
                                             height=plot_height())),
                  mod_Report_Add_Button_ui(ns(paste0('report_button_violin_', mm)))
          )
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    output$both <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("both", mm, sep="")
          tagList(loading_spinner(plotOutput(session$ns(plotname),
                                             width=plot_width(),
                                             height=plot_height())),
                  mod_Report_Add_Button_ui(ns(paste0('report_button_both_', mm)))
                  )
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    make_plots <- reactive({
      if (is.null(filtered_slick()))
        return(NULL)
      if (is.na(nPM()))
        return(NULL)
      dd <- filtered_slick() |> Boxplot() |> Value() |>
        dim()
      plot_list <- list()
      if (dd[4]==nPM() & length(selected_oms())>0) {
        for (i in 1:nPM()) {
          plot_list[[i]] <- plotBoxplot(filtered_slick(), i, 'all', byOM=TRUE)
        }
      }
      plot_list
    })

    observeEvent(make_plots(), {
      thisplot <- make_plots()
      for (i in 1:nPM()) {
        local({
          my_i <- i
          plotname <- paste("boxplot", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[1]]
          })

          plotname <- paste("violin", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[2]]
          })

          plotname <- paste("both", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[3]]
          })
        })
      }
    })
  })
}

## To be copied in the UI
# mod_Boxplot_OM_ui("Boxplot_OM_1")

## To be copied in the server
# mod_Boxplot_OM_server("Boxplot_OM_1")

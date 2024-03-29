

#' TradeOff_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TradeOff_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot'))
  )
}

#' TradeOff_plot Server Functions
#'
#' @noRd
mod_TradeOff_plot_server <- function(id, i18n, Slick_Object, Filter_Selected){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_slick <- reactive({
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      selected_MPs <- Filter_Selected$MPs
      # selected_PMs <- Filter_Selected$PMs
      quilt <- Quilt(slick)

      # filter OMs
      if (!is.null(selected_OMs)) {
        Value(quilt) <- Value(quilt)[selected_OMs,,, drop=FALSE]
      }
      # filter MPs
      if (!is.null(selected_MPs)) {
        Value(quilt) <- Value(quilt)[,selected_MPs,, drop=FALSE]
      }
      # filter PMs
      # if (!is.null(selected_PMs)) {
      #   Value(quilt) <- Value(quilt)[,,selected_PMs, drop=FALSE]
      # }
      quilt
    })

    nOM <- reactive({
      dim(Value(filtered_slick()))[1]
    })

    MP_obj <- reactive({
      MPs(Slick_Object())
    })

    MP_labels <- reactive({
      labels <- Label(MP_obj())
      labels[Filter_Selected$MPs]
    })

    nMP <- reactive({
      length(MP_labels())
    })

    PM_labels <- reactive({
      Label(Quilt(Slick_Object()))
    })

    output$plot <- renderUI({
      i18n <- i18n()
      quilt <- filtered_slick()
      tagList(
        br(),
        shinydashboard::box(width=12, collapsible = TRUE,
                            status='primary',
                            title=strong(i18n$t("READING THIS CHART")),
                            htmlOutput(ns('reading'))
        ),
        shinydashboard::box(width=12,
                            status='primary',
                            title=strong(paste(nMP(),
                                               i18n$t('Management Procedures. Median values over'),
                                               nOM(),
                                               i18n$t('Operating Models'))
                            ),
                            uiOutput(ns('tradeoff'))

        )
      )
    })


    output$tradeoff <- renderUI({
      i18n <- i18n()
      pm_labels <- PM_labels()
      tagList(
        column(3,
               shinyWidgets::pickerInput(
                 inputId = ns('xPM'),
                 label = i18n$t("X-Axis Performance Indicator"),
                 selected=pm_labels[1],
                 choices = pm_labels
               ),
               shinyWidgets::pickerInput(
                 inputId = ns('yPM'),
                 label = i18n$t("Y-Axis Performance Indicator"),
                 selected=pm_labels[2],
                 choices = pm_labels
               )
        ),
        column(9,
               plotOutput(ns('tradeoffplot'))
        )
      )
    })


      output$tradeoffplot <- renderPlot({
        make_TradeOff(filtered_slick(), MP_obj(), input$xPM, input$yPM)
      })


      make_TradeOff <- function(quilt, mp_obj, xaxis, yaxis) {
        quilt <<- quilt
        mp_obj <<- mp_obj
        xaxis <<- xaxis
        yaxis <<- yaxis

        Values <- Value(quilt) |>
          apply(2:3, median) |>
          signif(3)
        if (all(is.na(Values))) {
          return(NULL)
        }

        # min and max
        # colors for MPs

        mp_label <- Label(mp_obj)
        x_index <- match(xaxis, Label(quilt))
        y_index <- match(yaxis, Label(quilt))

        x_value <- Values[,x_index]
        y_value <- Values[, y_index]

        df <- data.frame(x=x_value, y=y_value, MP=mp_label)

        ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=MP)) +
          ggplot2::geom_point()

      }



    output$reading <- renderUI({

      tagList(
        p('This chart ...')

      )
    })
  })
}

## To be copied in the UI
# mod_TradeOff_plot_ui("TradeOff_plot_1")

## To be copied in the server
# mod_TradeOff_plot_server("TradeOff_plot_1")

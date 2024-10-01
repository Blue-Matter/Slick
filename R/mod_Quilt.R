

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
    # mod_toplink_ui(ns(id)),
    # mod_Report_Add_Button_ui(ns('report_button')),
    tagList(
      shinydashboardPlus::box(width=12,
                              status='primary',
                              solidHeader=TRUE,
                              title=h3(strong('Quilt')),
                              column(12, align = 'left', class='multicol',
                                     tagList(
                                       br(),
                                       column(12, mod_subtitle_ui(ns('quiltsubtitle'))
                                       ),
                                       column(3,
                                              uiOutput(ns('reading_settings'))
                                       ),
                                       column(9,
                                              mod_Report_Add_Button_ui(ns('report_button')),
                                              br(),
                                              uiOutput(ns('quilttable'))
                                       )
                                     )
                              )
      )
    )
  )
}

#' Quilt Server Functions
#' @noRd
mod_Quilt_server <- function(id, i18n, Slick_Object, window_dims, Report, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Plot_Object <- reactiveVal()
    quilt_slick <- reactiveVal()

    # mod_Report_Add_server("Report_Add_2", i18n, parent_session=parent_session,
    #                       Report,
    #                       Plot_Object=Plot_Object,
    #                       'Quilt',
    #                       window_dims)
    #
    # button_pushed <- mod_Report_Add_Button_server("report_button", i18n)
    #
    # observeEvent(button_pushed(), {
    #   Plot_Object(plotQuilt(quilt_slick(), kable=TRUE))
    #
    #   if(!inherits(Plot_Object(), 'NULL'))
    #     shiny::showModal(mod_Report_Add_ui(ns("Report_Add_2")))
    # })


    mod_subtitle_server('quiltsubtitle', i18n, nOM, nMP, nPM, minPM=1, OMtext=OMtext)

    OMtext <- reactive({
      'over'
    })

    Filter_Selected <- mod_Page_Filter_server("quiltfilter",i18n, Slick_Object,
                                               slot='Quilt', minPM=2, incIcons=FALSE,
                                               home_session=home_session)


    filtered_slick <- reactive({
      FilterSlick(Slick_Object(),
                  as.numeric(Filter_Selected$MPs),
                  as.numeric(Filter_Selected$OMs),
                  as.numeric(Filter_Selected$PMs),
                  'Quilt')
    })

    observeEvent(filtered_slick(), {
      quilt_slick(filtered_slick())
    })

    output$reading_settings <- renderUI({
      i18n <- i18n()
      tagList(
        h4(strong(i18n$t("Reading this Chart"))),
        htmlOutput(ns('reading')),
        mod_Page_Filter_ui(ns("quiltfilter")),
        uiOutput(ns('quiltcolors')),
        uiOutput(ns('minmaxoption'))
      )
    })

    output$minmaxoption <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          column(4,
                 checkboxInput(ns('shading'),
                               i18n$t('Shading?'),
                               TRUE)
                 ),
          column(4,
                 checkboxInput(ns('minmax'),
                               i18n$t('Min-Max Color Scaling?'),
                               FALSE)
                 ),
          column(4,
                 checkboxInput(ns('static'),
                               i18n$t('Static Table?'),
                               FALSE)
          )
        )
      )
    })

    MinMax <- reactive({
      out <- input$minmax
      if (length(out)<1) return(FALSE)
      out
    })

    Shading <- reactive({
      out <- input$shading
      if (length(out)<1) return(TRUE)
      out
    })
    Static <- reactive({
      out <- input$static
      if (length(out)<1) return(FALSE)
      out
    })

    values <- reactive({
      quilt_slick() |>
        Quilt() |>
        Value()
    })

    dims <- reactive({
      values() |> dim()
    })

    nOM <- reactive({
      dims()[1]
    })

    nMP <- reactive({
      dims()[2]
    })

    nPM <- reactive({
      dims()[3]
    })

    output$page <- renderUI({
      req(quilt_slick())
      chk <- Check(quilt_slick())
      if (chk@empty$Quilt) return(NULL)
      i18n <- i18n()

    })

    output$quiltcolors <- renderUI({
      i18n <- i18n()
      tagList(
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel(title=i18n$t('Quilt Color Settings'),
                                   value='color',
                                   tagList(
                                     uiOutput(ns('colorpickerselect')),
                                     shinyWidgets::actionBttn(ns("applycolors"),
                                                              label=i18n$t("Apply Color Selections"),
                                                              icon("gear", verify_fa=FALSE),
                                                              color='danger',size='sm',
                                                              block=T, style="fill")
                                   )
          )
        )
      )
    })

    output$colorpickerselect <- renderUI({
      i18n <- i18n()
      slick <- quilt_slick()
      cols <- Color(Quilt(slick))

      tagList(
        colourpicker::colourInput(ns('highcolor'),
                                  i18n$t('High Values'),
                                  value=cols[1]),
        colourpicker::colourInput(ns('lowcolor'),
                                  i18n$t('Low Values'),
                                  value=cols[2])
      )
    })

    observeEvent(input$applycolors, {
      newcols <- c(input$highcolor, input$lowcolor)
      slick <- quilt_slick()
      quilt <- Quilt(slick)
      Color(quilt) <- newcols
      slick@Quilt <- quilt
      quilt_slick(slick)
    })

    output$quilttable <- renderUI({
      static <- Static()
      if (length(static)<1)
        static <- FALSE

      if (static) {
        return(
          htmlOutput(ns('quilttableKable'))
        )
      } else {
        return(
          DT::dataTableOutput(ns('quilttableDT'))
        )
      }
    })

    output$quilttableKable <- renderUI({
      req(quilt_slick)
      plotQuilt(quilt_slick(), minmax = MinMax(), shading=Shading(), kable=TRUE) |>
        flextable::autofit() |> flextable::htmltools_value()
    })


    output$quilttableDT <- DT::renderDataTable({
      req(quilt_slick)
      plotQuilt(quilt_slick(), minmax = MinMax(), shading=Shading())
      # plot_object() |> flextable::autofit() |> flextable::htmltools_value()
    })


    output$reading <- renderUI({
      i18n <- i18n()
      tagList(
        p(i18n$t('The Quilt plot is designed as a color-coded table showing the values of the selected Performance Indicators (columns) for each Management Procedure (rows).')),
        # p(i18n$t(
        #   'The color shading indicates values ranging between the value to the '
        # ))
        p(uiOutput(ns('colortext'))),

        p(i18n$t('An exception is if the analyst who built the Slick Object specified maximum and minimum values for each performance indicator (e.g., between 0 and 1 for probabilities).')),

        p(i18n$t('Use the button below the Quilt table to modify the colors shown in the table.')),

        p(i18n$t('Note: currently the Report feature is not implemented for the Quilt plot.'))
      )
    })

    output$colortext <- renderUI({
      i18n <- i18n()
      hightxt <- i18n$t("highest value")
      lowtxt <- i18n$t(" lowest value ")
      HTML(paste(i18n$t('The color shading indicates values ranging between the'),
                 span(hightxt, style=highcolorstyle()),
                 i18n$t('to the'),
                 span(lowtxt, style=lowcolorstyle()),
                 i18n$t('for the performance indicator in each column. Higher values indicated better performance.')
      )
      )
    })

    colors <- reactive({
      quilt_slick() |> Quilt() |> Color()
    })

    highcolorstyle <- reactive({
      # paste('color:', paste0(colors()[1], 99))
      paste('color:', colors()[1])
    })
    lowcolorstyle <- reactive({
      # paste('color:', paste0(colors()[2],99))
      paste('color:', colors()[2], '; background-color: gray')
    })

  })
}

## To be copied in the UI
# mod_Quilt_ui("Quilt")

## To be copied in the server
# mod_Quilt_server("Quilt")



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
    uiOutput(ns('page'))
  )
}

#' Quilt Server Functions
#' @noRd
mod_Quilt_server <- function(id, i18n, Slick_Object, window_dims, Report, home_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # mod_toplink_server(id, links=list(hometab='Home',
    #                                   metadatatab='Overview',
    #                                   quilt='Quilt'))

    mod_subtitle_server('quiltsubtitle', i18n, nOM, nMP, nPM, minPM=1, OMtext=OMtext)

    OMtext <- reactive({
      'over'
    })

    Filter_Selected <- mod_Page_Filter_server("quiltfilter",i18n, Slick_Object,
                                               slot='Quilt', minPM=2, incIcons=FALSE,
                                               home_session=home_session)

    quilt_slick <- reactiveVal()

    observeEvent(Slick_Object(), {
      quilt_slick(Slick_Object())
    })

    observeEvent(Slick_Object(), {
      filtered_slick()
    })

    filtered_slick <- reactive({
      FilterSlick(quilt_slick(),
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
      req(Slick_Object())
      chk <- Check(Slick_Object())
      if (chk@empty$Quilt) return(NULL)
      i18n <- i18n()
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
                                                h4(strong(i18n$t("Reading this Chart"))),
                                                htmlOutput(ns('reading')),
                                                mod_Page_Filter_ui(ns("quiltfilter")),
                                                uiOutput(ns('quiltcolors'))
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
      slick <- Slick_Object()
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
      slick <- filtered_slick()
      quilt <- Quilt(slick)
      Color(quilt) <- newcols
      Quilt(slick) <- quilt
      quilt_slick(slick)
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
        p(i18n$t('The Quilt plot is designed as a color-coded table showing the values of the selected Performance Indicators (columns) for each Management Procedure (rows).')),
        p(uiOutput(ns('colortext'))),

        p(i18n$t('Use the button below the Quilt table to modify the colors shown in the table.'))
      )
    })

    output$colortext <- renderUI({
      i18n <- i18n()
      hightxt <- i18n$t("highest value")
      lowtxt <- i18n$t("lowest possible value (0)")
      HTML(paste(i18n$t('The color shading indicates values ranging between the'),
                 span(hightxt, style=highcolorstyle()),
                 i18n$t('to the'),
                 span(lowtxt, style=lowcolorstyle()),
                 i18n$t('for the performance indicator in each column. Higher values indicated better performance.')
      )
      )
    })

    colors <- reactive({
      filtered_slick() |> Quilt() |> Color()
    })

    highcolorstyle <- reactive({
      paste('color:', paste0(colors()[1], 99))
      # paste('color:', colors()[1])
    })
    lowcolorstyle <- reactive({
      paste('color:', paste0(colors()[2],99))
      # paste('color:', colors()[2])
    })


    nOM <- reactive({
      dd <- filtered_slick() |> Quilt() |> Value() |> dim()
      dd[1]
    })

    MP_labels <- reactive({
      slick <- Slick_Object()
      selectedmps <- as.numeric(Filter_Selected$MPs)
      metadata <- Metadata(MPs(slick))
      metadata$Code[selectedmps]
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


# stop()
#
# TODO: Fix the buttons resetting once the filters are changed!!

#' Spider_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Spider_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('plot'))
  )
}

#' Spider_plot Server Functions
#'
#' @noRd
mod_Spider_plot_server <- function(id, i18n, Slick_Object, Filter_Selected,
                                   parent_session=session, window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_slick <- reactive({
      if (is.null(Slick_Object())) return(NULL)
      slick <- Slick_Object()
      selected_OMs <- Filter_Selected$OMs
      selected_MPs <- Filter_Selected$MPs
      selected_PMs <- Filter_Selected$PMs
      spider <- Spider(slick)

      dd <- dim(Value(spider))
      if (length(selected_OMs)==dd[1]) {
        # filter OMs
        if (!is.null(selected_OMs)) {
          Value(spider) <- Value(spider)[selected_OMs,,, drop=FALSE]
        }
        # filter MPs
        if (!is.null(selected_MPs)) {
          Value(spider) <- Value(spider)[,selected_MPs,, drop=FALSE]
          metadata <- Metadata(MPs(slick))
          Metadata(MPs(slick)) <- metadata[selected_MPs,]
        }
        # filter PMs
        if (!is.null(selected_PMs)) {
          Metadata(spider) <- Metadata(spider)[selected_PMs, ]
          Value(spider) <- Value(spider)[,,selected_PMs, drop=FALSE]
        }
      }
      Spider(slick) <- spider
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

    mod_subtitle_server(id, i18n, nOM, nMP, nPM)

    mod_Spider_MP_server("Spider_MP_1", i18n, filtered_slick,
                         nOM, nMP, nPM, parent_session,
                         relative_scale=relative_scale)
    mod_Spider_OM_server("Spider_OM_1", i18n, filtered_slick,
                         nOM, nMP, nPM, parent_session,
                         relative_scale=relative_scale)

    mod_Spider_overall_server("Spider_overall_1",
                              i18n, filtered_slick,
                              nOM, nMP, nPM, parent_session,
                              relative_scale=relative_scale,
                              window_dims)


    get_relative_scale_md <- reactive({
      lang <- i18n()$get_translation_language()
      paste('relative_scale', lang, sep='_')
    })

    output$plot <- renderUI({
      i18n <- i18n()
      tagList(
        br(),
        column(12,
               mod_subtitle_ui(ns(id))
        ),
        column(6,
               shinyWidgets::radioGroupButtons(
                 inputId = ns("plotselect"),
                 choiceNames = c(i18n$t('Overall'),
                                 i18n$t('By Management Procedure'),
                                 i18n$t('By Operating Model')
                                 ),
                 choiceValues=c('overall', 'bymp',  'byom'),
                 justified = TRUE
               )
        ),
        column(6,
               div(
                 shinyhelper::helper(
                   shinyWidgets::switchInput(
                     inputId = ns("RS_button"),
                     handleWidth = 60,
                     labelWidth = 100,
                     inline=TRUE,
                     label=i18n$t('Relative Scale'),
                     width = "auto"
                   ),
                   content = get_relative_scale_md(),
                   size='s'),
                 style='float:right')
        ),
        column(12,
               conditionalPanel("input.plotselect=='overall'", ns=ns,
                                mod_Spider_overall_ui(ns("Spider_overall_1"))
               ),
               conditionalPanel("input.plotselect=='bymp'", ns=ns,
                                mod_Spider_MP_ui(ns("Spider_MP_1"))
               ),
               conditionalPanel("input.plotselect=='byom'", ns=ns,
                                mod_Spider_OM_ui(ns("Spider_OM_1"))
               )
        )
      )
    })

    relative_scale <- reactive({
      input$RS_button
    })


    # output$mainplot <- renderUI({
    #   tagList(
    #     conditionalPanel(condtion="input.plotselect=='bymp'", ns=ns,
    #
    #                      mod_Spider_MP_ui(ns("Spider_MP_1"))
    #     ),
    #     conditionalPanel(condtion="input.plotselect=='byom'", ns=ns,
    #                      p('By OM!')
    #     ),
    #     conditionalPanel(condtion="input.plotselect=='overall'", ns=ns,
    #                      p('Overall!')
    #     )
    #   )
    # })



  })
}

## To be copied in the UI
# mod_Spider_plot_ui("Spider_plot_1")

## To be copied in the server
# mod_Spider_plot_server("Spider_plot_1")

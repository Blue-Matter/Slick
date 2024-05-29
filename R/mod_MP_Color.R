#' MP_Color UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MP_Color_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('page'))
  )
}

#' MP_Color Server Functions
#'
#' @noRd
mod_MP_Color_server <- function(id, i18n, slick){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$page <- renderUI({
      i18n <- i18n()
      # tagList(
      #   column(12,
      #          h4(i18n$t('MP Color Settings')),
      #          p(i18n$t('Modify the colors of the MP or select from an existing palette')),
      #          uiOutput(ns('colorpickerselect')),
      #          uiOutput(ns('customcolors')),
      #          shinyWidgets::actionBttn(ns("applycolors"),
      #                                   label=i18n$t("Apply Color Selections"),
      #                                   icon("gear", verify_fa=FALSE),
      #                                   color='danger',size='sm',
      #                                   block=T, style="fill")
      #          )
      # )

      tagList(
        column(12,
               br(),
               shinyBS::bsCollapse(id=ns('mpcolorcollapse'),
                 shinyBS::bsCollapsePanel(title=i18n$t('MP Color Settings'),
                                          value='color',
                                          tagList(
                                            p(i18n$t('Click the colored box of each MP to select a different color, or select from an existing color palette at the bottom.')),
                                            uiOutput(ns('customcolors')),
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
      )
    })

    slick_object <- reactiveVal()

    observeEvent(slick(),
                 slick_object(slick())
                 )

    mp_cols_default <- reactive({
      metadata <- slick() |> MPs() |> Metadata()
      metadata$Color
    })

    mp_metadata_all <- reactive({
      slick() |> MPs() |> Metadata()
    })

    mp_metadata <- reactive({
      slick_object() |> MPs() |> Metadata()
    })

    mp_names <- reactive({
      mp_metadata()$Code
    })

    mp_cols <- reactive({
      mp_metadata()$Color
    })

    nMPs <- reactive({
      mp_metadata_all() |> nrow()
    })



    palette_list <- reactive({
      ll <- list()
      palettes <- c('Default', 'ArmyRose', 'Earth', 'Fall', 'Geyser', 'TealRose', 'Temps', 'Tropic')
      for (l in 2:length(palettes)) {
        ll[[l]] <- colorspace::divergex_hcl(nMPs(), palettes[l])
      }
      ll[[1]] <- mp_cols_default()
      names(ll) <- palettes
      ll
    })

    output$colorpickerselect <- renderUI({
      i18n <- i18n()
      tagList(
        esquisse::palettePicker(ns('colorpicker'),
                                i18n$t('Select a Color Palette'),
                                choices=palette_list())
      )
    })


    observeEvent(input$colorpicker, {
      slick <- slick_object()
      metadata <- slick |> MPs() |> Metadata()
      metadata$Color <- palette_list()[[input$colorpicker]]
      Metadata(MPs(slick)) <- metadata
      slick_object(slick)

    })

    output$customcolors <- renderUI({
      i18n <- i18n()
      metadata <- mp_metadata()
      tagList(
        mp_color_list()
      )
    })

    selected_colors <- reactiveVal()

    mp_color_list <- reactive({
      nMP <- nMPs()
      metadata <- mp_metadata()
      ll <- list()
      for (i in 1:nMP) {
        ll[[i]] <- colourpicker::colourInput(ns(paste0('mpcol',i)),
                                             metadata$Code[i],
                                             value=metadata$Color[i])
      }
      do.call(tagList, ll)
    })


    observeEvent(input$applycolors, {
      # shinyBS::updateCollapse(session=session, id='mpcolorcollapse',
      #                         close='color')
      colind <-  paste0("mpcol",1:nMPs())
      cols <- sapply(colind, function(x) input[[x]])
      selected_colors(cols)

    })

    selected_colors
  })
}

## To be copied in the UI
# mod_MP_Color_ui("MP_Color_1")

## To be copied in the server
# mod_MP_Color_server("MP_Color_1")

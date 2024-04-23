options(shiny.maxRequestSize=100000*1024^2)


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # helper
  shinyhelper::observe_helpers(help_dir=file.path(app_sys(), 'app/helpfiles'))


  # dimensions of the brower window
  window_dims <- reactive(input$dimension)

  # -- multi-language support ----
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% set_translator()$get_languages()) {
      set_translator()$set_translation_language(selected)
    }
    set_translator()
  })

  output$language <- renderUI({
    tagList(
      selectInput('selected_language',
                  "Choose Language",
                  choices = get_languages()
                  )
      )
  })

  outputOptions(output, "language", suspendWhenHidden = FALSE)

  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language, session)
    i18n()$set_translation_language(input$selected_language)
    # i18n <- i18n()
    # updateSelectInput(inputId='selected_language', label=i18n$t("Choose Language"))
  })

  # ---- Reactives -----
  Load_Slick_File <- reactiveValues(loaded=FALSE, file=NULL)
  Slick_Object <- reactiveVal()

  output$Loaded <- reactive({ Load_Slick_File$loaded })
  outputOptions(output, "Loaded", suspendWhenHidden = FALSE)

  ## ---- Report ----
  Report <- reactiveValues(Metadata=list(),
                           Quilt=list(plot=list(), caption=list()),
                           Tradeoff=list(plot=list(), caption=list()),
                           Spider=list(plot=list(), caption=list()),
                           Kobe=list(plot=list(), caption=list()),
                           Timeseries=list(plot=list(), caption=list()))


  # Report - Metadata & MP info etc
  observeEvent(Slick_Object(), {
    i18n <- i18n()
    slick <- Slick_Object()
    Report$Metadata <- list(Title=(Title(slick, i18n$get_translation_language())),
                         Subtitle=Subtitle(slick, i18n$get_translation_language()),
                         Author=Author(slick),
                         Introduction=Introduction(slick))

    Report$Quilt <- list(plot=list(), caption=list())
    Report$Tradeoff <- list(plot=list(), caption=list())
    Report$Spider <- list(plot=list(), caption=list())
    Report$Kobe <- list(plot=list(), caption=list())
    Report$Timeseries <- list(plot=list(), caption=list())

  })

  # ---- Module Servers ----
  waitress <- waiter::Waitress$new(theme = "overlay-percent") # call the waitress
  waitress$set(5)
  mod_Resources_server('resources', i18n)

  mod_About_server("about", i18n)
  mod_Sidebar_server("sidebar", i18n, Load_Slick_File)
  mod_Home_server("home", i18n, Load_Slick_File, Slick_Object, Report)
  mod_Report_Page_server('Report_Page_1', i18n, Slick_Object, Report)

  waitress$inc(5)

  mod_Metadata_server("metadata", i18n, Slick_Object)
  mod_MP_Info_server("MPheader", i18n, Slick_Object)
  mod_OM_Info_server("OMheader", i18n, Slick_Object)
  mod_PM_Info_server("PMheader", i18n, Slick_Object)

  waitress$inc(5)

  mod_Boxplot_server("Boxplot", i18n, Slick_Object, window_dims, Report)
  waitress$inc(5)
  mod_Kobe_server("Kobe", i18n, Slick_Object, window_dims, Report)
  waitress$inc(5)
  mod_Quilt_server("Quilt", i18n, Slick_Object, window_dims, Report)
  waitress$inc(5)
  mod_Spider_server("Spider", i18n, Slick_Object, window_dims, Report)
  waitress$inc(5)
  mod_Timeseries_server("Timeseries_1", i18n, Slick_Object, window_dims, Report)
  waitress$inc(5)
  mod_Tradeoff_server("Tradeoff", i18n, Slick_Object, window_dims, Report)

  waitress$close()

}

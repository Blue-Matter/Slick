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

  # ---- Module Servers ----
  mod_Resources_server('resources', i18n)
  mod_About_server("about", i18n)
  mod_Sidebar_server("sidebar", i18n, Load_Slick_File)
  mod_Home_server("home", i18n, Load_Slick_File, Slick_Object)
  mod_Metadata_server("metadata", i18n, Slick_Object)
  mod_MP_Info_server("MPheader", i18n, Slick_Object)
  mod_OM_Info_server("OMheader", i18n, Slick_Object)
  mod_PM_Info_server("PMheader", i18n, Slick_Object)
  mod_Quilt_server("Quilt", i18n, Slick_Object, window_dims)
  mod_Spider_server("Spider", i18n, Slick_Object, window_dims)

}

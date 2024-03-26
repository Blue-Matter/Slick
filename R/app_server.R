options(shiny.maxRequestSize=100000*1024^2)


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # -- multi-language support ----

  output$language <- renderUI({
    tagList(
      selectInput('selected_language',
                  "Select language",
                  choices = get_languages(),
                  selected = set_translator()$get_key_translation()
      )
    )
  })

  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% set_translator()$get_languages()) {
      set_translator()$set_translation_language(selected)
    }
    set_translator()
  })

  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language, session)
  }, ignoreInit = TRUE)

  # ---- Reactives -----
  Load_Slick_File <- reactiveValues(loaded=FALSE, file=NULL)
  Slick_Object <- reactiveVal()

  # ---- Module Servers ----
  mod_Resources_server('resources', i18n)
  mod_About_server("about", i18n)
  mod_Filters_server("filters", i18n)
  mod_Sidebar_server("sidebar", i18n, Load_Slick_File)
  mod_Home_server("home", i18n, Load_Slick_File, Slick_Object)
  mod_Metadata_server("metadata", i18n, Slick_Object)

}


# -- multi-language support ----


languageButton_UI <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    usei18n(i18n),
    dropdownButton(
      label = "Switch Language",
      icon = icon("language"),
      status = "primary",
      circle = FALSE,
      selectInput('selected_language',
                  i18n$t("Select language"),
                  choices = languages,
                  selected = i18n$get_key_translation())
    )
  )

}


languageButton_Server <- function(id, global_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      observeEvent(input$selected_language, {
        # Here is where we update language in session
        shiny.i18n::update_lang(global_session, input$selected_language)
      })
    }
  )
}


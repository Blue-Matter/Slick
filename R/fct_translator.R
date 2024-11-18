#' translator
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
set_translator <- function() {
  i18n  <- shiny.i18n::Translator$new(translation_csvs_path = system.file('translations', package = 'Slick'),
                                      translation_csv_config = system.file('config.yaml', package = 'Slick'))
  # i18n$set_translation_language('en')
  i18n$use_js()
  i18n
}


get_languages <- function() {
  languages <- set_translator()$get_languages()
  ind <- match(languages, language_codes[,1])
  lang_names <- language_codes[ind,2]
  lang_names[3] <- paste(lang_names[3], '(Under Construction)')
  names(languages) <- lang_names
  languages
}




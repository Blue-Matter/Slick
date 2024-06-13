
lang_param <- function() {
  "Optional text string specifying the language (if available).
  Either 'en', 'es', or 'fr' for English, Spanish, or French respectively"
}

type_param <- function() {
  "Required if `object` is class [Slick-class()]. One of c('MPs',  'Boxplot',
  'Kobe','Quilt', 'Spider', 'Timeseries', 'Tradeoff')"
}

object_Code_param <- function() {
  "An object of class [MPs-class()], [Boxplot-class()],
  [Kobe-class()], [Quilt-class()], [Spider-class()],
  [Timeseries-class()], or [Tradeoff-class()]
  "
}

object_Metadata_param <- function() {
  "An object of class [MPs-class()], [OMs-class()], [Boxplot-class()],
  [Kobe-class()], [Quilt-class()], [Spider-class()],
  [Timeseries-class()], or [Tradeoff-class()]
  "
}

code_PI_param <- function() {
  "A *short* code for the Performance Indicators for this object.
   A character string length `nPI` or a named list for multi-language support. See `Details`
  "
}
label_PI_param <- function() {
  "A short label for the Performance Indicators for this object. Used to label axes on charts.
   Can be longer than `Code` but recommended to keep short as possible so it shows clearly in plots and tables.
   A character string length `nPI` or a named list for multi-language support. See `Details`
  "
}

description_PI_param <- function() {
  "A description for the Performance Indicators for this object.
   Can include Markdown, see `Examples`.
  A character string length `nPI` or a named list for multi-language support. See `Details`"
}

code_MP_param <- function() {
  "A *short* code for the Management Procedures in this `Slick` object.
   A character string length `nMP` or a named list for multi-language support. See `Details`
  "
}
label_MP_param <- function() {
  "A short label for the  Management Procedures in this `Slick` object.
   Can be longer than `Code` but recommended to keep short as possible so it shows clearly in plots and tables.
   A character string length `nMP` or a named list for multi-language support. See `Details`
  "
}

description_MP_param <- function() {
  "A description for the Management Procedures in this `Slick` object.
   Can include Markdown, see `Examples`.
  A character string length `nMP` or a named list for multi-language support. See `Details`"
}


preset_param <- function() {
  "An optional named list for the preset buttons in the [App()]. The name of the list
  element will appear as a button in the [App()]."
}

title_param <- function() {
  "Title for the `Slick` object. A character string. For multiple languages,
  use a named list with names: `en`, `es`, `fr` for the three supported languages."
}

subtitle_param <- function() {
  "
  Subtitle for the `Slick` object. A character string or a named list with
   languages: `en`, `es`, `fr`
  "
}

date_param <- function() {
  "
  Date the Slick object was created. Text in format 'YYYY-MM-DD' or class `Date` e.g., `Sys.Date()`
  "
}

author_param <- function() {
  "
  A character vector with Author(s) names. The length of the vector should equal the number of authors.
  "
}

email_param <- function() {
  "
  A character vector with email addresses for the author(s). Must be same length as `Author`. Can include Markdown.
  "
}

institution_param <- function() {
  "
  A character vector with institution details for the author(s). Must be same length as `Author`. Can include Markdown.
  "
}

introduction_param <- function() {
  "
  Introduction text for the `Slick` object. Supports all markdown formatting. Character string, must be length 1.
  For multiple languages, use a named list with names: `en`, `es`, `fr` for the three supported languages.
  "
}

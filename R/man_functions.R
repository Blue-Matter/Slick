
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
  [Timeseries-class()], [Tradeoff-class()], or [Slick-class()]
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

preset_param <- function() {
  "An optional named list for the preset buttons in the [App()]. See `Examples`"
}

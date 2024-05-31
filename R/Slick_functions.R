process_markdown <- function(val, markdown) {
  if (markdown) val <- shiny::markdown(val)
  val
}


#' Slick Metadata Information
#'
#' Access or assign the metadata for a [Slick-class()] object
#'
#' @param Slick An object of class [Slick-class()]
#' @param lang `r lang_param()`
#' @param markdown Logical. Process markdown?
#' @param value A value to assign to the `Slick` object. See `Details` and `Examples`
#'
#' @rdname Slick-metadata
#' @example inst/examples/Slick_Info.R
#'
#' @details
#' All functions with the exception of `Date` support Markdown.
#'
#' ## Multi-Langauge Support
#' `Title`, `Subtitle`, and `Introduction` have multi-language support.
#'
#'  Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' See `Examples`
#'
#' ## Assigning Values
#' - `Title` A character string or named list with the title of the Slick object
#' - `Subtitle` A character string or named list with the subtitle of the Slick object
#' - `Date` Date the Slick object was created as text in format 'YYYY-MM-DD' or class `Date` e.g., `Sys.Date()`
#' - `Author` A character string with Author(s) names. Should be length `nAuthors`
#' - `Email` The email address corresponding with each author. Should be length `nAuthors`
#' - `Institution` The institution corresponding with each author. Should be length `nAuthors`
#' - `Introduction` Character string with introduction describing the Slick object. Length 1. See `Examples`
#'
#' @name Slick-functions
NULL


#' @describeIn Slick-metadata Access `Title`, Multi-language support
#' @export
Title <- function(Slick, lang='es', markdown=TRUE) {
  process_markdown(get_language(Slick@Title, lang), markdown)
}

#' @describeIn Slick-metadata Assign `Title`, Multi-language support
#' @export
`Title<-` <- function(Slick,  value) {
  Slick@Title <- value
  methods::validObject(Slick)
  Slick
}


#' @describeIn Slick-metadata Access `Subtitle`, Multi-language support
#' @export
Subtitle <- function(Slick, lang='es', markdown=TRUE) {
  process_markdown(get_language(Slick@Subtitle, lang), markdown)
}

#' @describeIn Slick-metadata Assign `Subtitle`, Multi-language support
#' @export
`Subtitle<-` <- function(Slick,  value) {
  Slick@Subtitle <- value
  methods::validObject(Slick)
  Slick
}

#' @describeIn Slick-metadata Access `Date`
#' @export
Date <- function(Slick) {
  Slick@Date
}

#' @describeIn Slick-metadata Assign `Date`
#' @export
`Date<-` <- function(Slick, value) {
  if (is.null(value)) return(Slick)
  if (inherits(value, 'POSIXct'))
    value <- value |> as.Date() |> as.character()

  Slick@Date <- value
  methods::validObject(Slick)
  Slick
}


#' @describeIn Slick-metadata Access `Author`
#' @export
Author <- function(Slick, markdown=TRUE) {
  process_markdown(Slick@Author, markdown)
}

#' @describeIn Slick-metadata Assign `Author`
#' @export
`Author<-` <- function(Slick,  value) {
  Slick@Author <- value
  methods::validObject(Slick)
  Slick
}


#' @describeIn Slick-metadata Access `Email`
#' @export
Email <- function(Slick, markdown=TRUE) {
  process_markdown(Slick@Email, markdown)
}

#' @describeIn Slick-metadata Assign `Email`
#' @export
`Email<-` <- function(Slick,  value) {
  Slick@Email <- value
  methods::validObject(Slick)
  Slick
}


#' @describeIn Slick-metadata Access `Institution`
#' @export
Institution <- function(Slick, markdown=TRUE) {
  process_markdown(get_language(Slick@Institution, lang), markdown)
}

#' @describeIn Slick-metadata Assign `Institution`
#' @export
`Institution<-` <- function(Slick,  value) {
  Slick@Institution <- value
  methods::validObject(Slick)
  Slick
}

#' @describeIn Slick-metadata Access `Introduction`
#' @export
Introduction <- function(Slick, lang='en', markdown=TRUE) {
  process_markdown(get_language(Slick@Introduction, lang), markdown)
}

#' @describeIn Slick-metadata Assign `Introduction`, can include Markdown. See `Examples`
#' @export
`Introduction<-` <- function(Slick,  value) {
  Slick@Introduction <- value
  methods::validObject(Slick)
  Slick
}




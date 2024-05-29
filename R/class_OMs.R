# ---- Class OMs ----

#' Create or modify an `OMs` object
#'
#' An `OMs` object contains information about the operating models (MPs)
#' in the [Slick()] object.
#'
#' `OMs()` creates an empty `OMs` object.
#'
#' Each slot of the `OMs` object can be accessed or modified by the corresponding
#' accessor function. See `Examples` sections below for more details.
#'
#' @param Metadata A data.frame with a specific structure, describing the details of
#' the operating models. See `Details` section
#'
#' @param Design A `data.frame` with the design matrix for the OMs. The columns
#' must be named with the factor names. The rows contain the factor levels for each OM. See `Details` section
#'
#' @param Preset An optional named list of preset buttons for filters in the [App()]. See `Details` section.
#'
#' @details
#'
#' ### Metadata
#' The `Metadata` data.frame must have n OMs rows and the following columns:
#'    - Code: A short character string for identifying the MPs. Used in the [App()] in places
#'    where there is no room for a full label.
#'    - Label: A long character string (one word or max two words) for identifying the MPs.
#'    - Description: A longer character string providing a description of the MPs.
#'    - Color: A character vector with the colors to use to display the MPs in the [App()]. Optional.
#'    If not provided, the App will try to set some sensible colors.
#'
#' ### Design
#'
#' ### Preset
#'
#'
#'
#'
#'
#' @return An object of class `OMs`
#' @usage OMs()
#' @export
#'
#' @example inst/examples/OMs.R
OMs <- setClass("OMs",
                slots=c(Metadata='dataframe_list',
                        Design='data.frame',
                        Preset='list'
                )
)

validOMs <- function(object) {

  # if (!is.null(Design(object))) {
  #   nfact <- ncol(Design(object))
  #   nlevels <- lapply(Design(object), unique) |>
  #     lapply(length) |>
  #     unlist()
  # }
  # if (!is.null(Description(object))) {
  #   if(length(Description(object)) != nfact)
  #
  #
  # }

  TRUE
}

setValidity('OMs', validOMs)

# initialize ----
setMethod("initialize", "OMs", function(.Object,
                                        Metadata=NULL,
                                        Design=NULL,
                                        Preset=NULL) {

  if (!is.null(Metadata)) {
    .Object@Metadata <- Metadata
  }

  if (!is.null(Design)) {
    if (!inherits(Design, 'data.frame'))
      stop('`Design` must be class `data.frame`', call.=FALSE)
    .Object@Design <- Design
  }

  if (!is.null(Preset)) {
    .Object@Preset <- Preset
  }


  .Object
})

newOMs <- function(Metadata=NULL,
                   Design=NULL,
                   Preset=NULL) {
  new('OMs', Metadata, Design, Preset)
}



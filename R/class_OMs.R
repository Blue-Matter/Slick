# ---- Class OMs ----

#' `OMs` S4 class and functions
#'
#' An `OMs` object contains information about the operating models (MPs)
#' in the [Slick()] object.
#'
#' @details
#' TODO
#'
#' ## Metadata
#' TODO
#'
#' ## Design
#' TODO
#'
#' ## Preset
#' TODO
#'
#' @slot Metadata TODO
#' @slot Design TODO
#' @slot Preset TODO
#'
#' @return An object of class `OMs`
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



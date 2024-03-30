# ---- Class OMs ----

#' Generic function for `OMs` objects
#'
#' Creates or populates a new `OMs` object, or returns or assigns the `OMs` slot in objects of class `Slick`
#'
#' Creates an empty `OMs` object, or specifies the design matrix, description, and labels of the operating models
#' (OMs) included in the Slick Data object.
#'
#' Also used to return or assign the `OMs` slot for objects of class `Slick`
#'
#' @param Design A `data.frame` with the design matrix for the OMs. The columns
#' must be named with the factor names. The rows contain the factor levels for each OM.
#' @param Description A list of length n factors (i.e., `ncol(Design)`), with each
#' element a character vector of length n levels (i.e., `lapply(lapply(Design, unique), length)`),
#' with a description of the factor levels.
#' @param Label (optional) A list with the same structure as `Description`,
#'  with a short label to be used in the plots. If missing, the label will be the
#'  factor level.
#' @param Default (optional) TODO
#'
#' @return An object of class `OMs`
#' @include aa_generics.R
#' @include class_Slick.R
#' @usage OMs(Design, Description, Label, Default)
#' @export
#'
#' @examples
#' Design <- expand.grid(M=c(0.1, 0.2, 0.3), h=c(0.7,0.9))
#' Description <- list(c('Natural mortality (M) = 0.1',
#'                       'Natural mortality (M) = 0.2',
#'                       'Natural mortality (M) = 0.3'),
#'                     c('Steepness (h) = 0.7',
#'                       'Steepness (h) = 0.9'))
#' Label <- list(c(M=0.1, M=0.2, M=0.3),
#'                c(h=0.7, h=0.9))
#' myOMs <- OMs(Design, Description, Label)
OMs <- setClass("OMs",
                slots=c(Metadata='dataframe_list',
                        Design='data.frame'
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

example_OMs_metadata <- data.frame(Factor=c(rep('Example.1', 2),
                                            rep('Example.2', 3)),
                                   Level=c(0.1,0.2, 10, 20, 30),
                                   Description=c('Description of Example.1 Level 1',
                                                 'Description of Example.1 Level 2',
                                                 'Description of Example.2 Level 1',
                                                 'Description of Example.2 Level 2',
                                                 'Description of Example.2 Level 2'),
                                   Default=c(TRUE, TRUE, TRUE, TRUE, FALSE)
                                   )

example_OMs_design <- data.frame(Example1=c(0.1,0.2),
                                 Example.2=c(rep(10,2), rep(20,2), rep(30,2))
)

# initialize ----
setMethod("initialize", "OMs", function(.Object,
                                        Metadata=NULL,
                                        Design=NULL) {

  if (!is.null(Metadata)) {
    .Object@Metadata <- Metadata
  }

  if (!is.null(Design)) {
    if (!inherits(Design, 'data.frame'))
      stop('`Design` must be class `data.frame`', call.=FALSE)
    .Object@Design <- Design
  }

  .Object
})

newOMs <- function(Metadata=NULL,
                   Design=NULL) {
  new('OMs', Metadata, Design)
}



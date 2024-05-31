
#' Assign or access the Operating Model Design matrix
#'
#
#' @param object An object of class [Slick()] or class [OMs()]
#' @examples
#' mySlick <- Slick()
#' Design(mySlick) <- data.frame(M=c(0.1, 0.2, 0.3),
#'                               h=c(0.6, 0.7, 0.8)
#'                              )
#' Design(mySlick)
#' @rdname Design
#' @export
Design <- function(Slick) {
  if (!inherits(Slick, 'Slick') | !inherits(Slick, 'OMs'))
    stop('Object must be class `Slick` or `OMs`')
  if (inherits(Slick, 'OMs')) {
    return(object@Design)
  }
  object@OMs@Design
}

#' @rdname Design
#' @param value A `data.frame` with the design matrix for the `OMs` object.
#' The columns must be named with the factor names. The rows contain the factor levels for each OM.
#' See [OMs] for more details.
#' @export
`Design<-` <- function(Slick, value) {
  if (!inherits(Slick, 'Slick') | !inherits(Slick, 'OMs'))
    stop('Object must be class `Slick` or `OMs`')
  if (inherits(Slick, 'OMs')) {
    Slick@OMs@Design <- value
  }
  Slick@Design <- value
  methods::validObject(Slick)
  Slick
}



#' Assign or access the `Colors` slot in an object of class `Quilt`
#' @rdname Colors
#' @param Quilt An object of class [Quilt()]
#' @export
#' @examples
#' quilt <- Quilt()
#' Colors(quilt) <- c('red', 'blue')
#' Colors(quilt)
#'
Colors <- function(Quilt) {
  Quilt@Colors
}

#' @rdname Colors
#' @param value A character vector of length 2 specifying the colors for
#' the maximum and minimum values in the Quilt chart
#' @export
#'
`Colors<-` <- function(Quilt,  value) {
  Quilt@Colors <- value
  methods::validObject(Quilt)
  Quilt
}


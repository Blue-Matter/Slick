
#' Access or assign `Code`, `Label`, and `Description` for a valid object class
#'
#' @details
#' *Note*:
#' If `object` is class [Slick-class()], then `type` must be specified.
#'
#' @param object `r object_Code_param()`
#' @param lang `r lang_param()`
#' @param type `r type_param()`
#'
#' @example inst/examples/Code.R
#'
#' @seealso [Label()], [Description()], [MPs-class()], [Boxplot-class()],
#' [Kobe-class()], [Quilt-class()], [Spider-class()],
#' [Timeseries-class()], [Tradeoff-class()], [Slick-class()]
#' @export
Code <- function(object, lang='en', type=NULL) {
  classes <- c('MPs',
               'Boxplot',
               'Kobe',
               'Quilt',
               'Spider',
               'Timeseries',
               'Tradeoff',
               'Slick')
  cl <- class(object)
  if (!cl %in% classes)
    stop('`object` must be one of the following classes: ',
         paste(classes, collapse=', '))
  if (cl =='Slick') {
    if (is.null(type))  {
      stop('`type` must be specified if `object` is class `Slick`. \nSelect from: ',
           paste(classes[-length(classes)], collapse=', '))
    }
    return(get_language(slot(object, type)@Code, lang))
  }
  get_language(object@Code, lang)
}

#' @describeIn Code Assign a replacement value for `Code` in `object`.
#' @param value The replacement value for `object`.
#' See the documentation in the corresponding `object` class for details.
#' @export
`Code<-` <- function(object, value, type=NULL) {

  classes <- c('MPs',
               'Boxplot',
               'Kobe',
               'Quilt',
               'Spider',
               'Timeseries',
               'Tradeoff',
               'Slick')

  cl <- class(object)
  if (!cl %in% classes)
    stop('`object` must be one of the following classes: ',
         paste(classes, collapse=', '))
  if (cl =='Slick') {
    if (is.null(type))  {
      stop('`type` must be specified if `object` is class `Slick`. \nSelect from: ',
           paste(classes[-length(classes)], collapse=', '))
    }

    slot(object, type)@Code <- value
  } else {
    object@Code <- value
  }
  object
}

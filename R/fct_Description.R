
#' @describeIn Code Access `Description` from a valid object class
#' @export
Description <- function(object, lang='en', type=NULL) {
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
    return(get_language(slot(object, type)@Description, lang))
  }
  get_language(object@Description, lang)
}

#' @describeIn Code Assign a replacement value for `Description` in `object`.
#' @export
`Description<-` <- function(object, value, type=NULL) {

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

    slot(object, type)@Description <- value
  } else {
    object@Description <- value
  }
  object
}

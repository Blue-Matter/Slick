

#' Return Metadata Information
#'
#' Creates a data.frame with `Code`, `Label` and `Description` for objects of class
#' `MPs`, and all chart types. For objects of class `OMs`, it returns a data.frame with
#' `Factor`, `Level` and `Description`
#'
#' @param object `r object_Metadata_param()`
#' @param lang `r lang_param()`
#'
#' @export
Metadata <- function(object, lang='en') {
  classes <- c('MPs',
               'Boxplot',
               'Kobe',
               'Quilt',
               'Spider',
               'Timeseries',
               'Tradeoff',
               'OMs')
  cl <- class(object)
  if (!cl %in% classes)
    stop('`object` must be one of the following classes: ',
         paste(classes, collapse=', '))


  if (cl =='OMs') {
    if (is.null(type))  {
      stop('`type` must be specified if `object` is class `OMs`. \nSelect from: c("factor", "design") ')
    }
    Factor <- get_language(object@Factor, lang)
    Level <-  get_language(object@Level, lang)
    Description <-  get_language(object@Description, lang)
    return(data.frame(Factor=Factor, Level=Level, Description=Description))
  }

  Code <- get_language(object@Code, lang)
  Label <-  get_language(object@Label, lang)
  Description <-  get_language(object@Description, lang)
  data.frame(Code=Code, Label=Label, Description=Description)
}

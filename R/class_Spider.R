#' Create a new `Spider` object
#'
#' @return An object of class `Spider`
#' @export
Spider <- setClass("Spider",
                   slots=c(Label='character_list',
                           Description='character_list',
                           Value='list'
                   )
)

setMethod("initialize", "Spider", function(.Object,
                                           Label=NULL,
                                           Description=NULL,
                                           Value=NULL) {
  .Object@Label <- use_ifnot_NULL('Label', Label, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object
})


validSpider <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Spider', validSpider)


newSpider <- function(Label=NULL,
                      Description=NULL,
                      Value=NULL) {
  Spider <- new('Spider', Label, Description, Value)
  validObject(Spider)
  Spider
}

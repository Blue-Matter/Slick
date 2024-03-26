# ---- Class Quilt ----
#' Create a new `Quilt` object
#'
#' @return An object of class `Quilt`
#' @export
#'
Quilt <- setClass("Quilt",
                  slots=c(Label='character_list',
                          Description='character_list',
                          Value='array'
                  )
)

setMethod("initialize", "Quilt", function(.Object,
                                          Label=NULL,
                                          Description=NULL,
                                          Value=NULL) {
  .Object@Label <- use_ifnot_NULL('Label', Label, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object
})


validQuilt <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Quilt', validQuilt)



newQuilt <- function(Label=NULL,
                     Description=NULL,
                     Value=NULL) {
  Quilt <- new('Quilt', Label, Description, Value)
  validObject(Quilt)
  Quilt
}

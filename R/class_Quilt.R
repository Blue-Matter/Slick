# ---- Class Quilt ----
#' Create a new `Quilt` object
#'
#' @return An object of class `Quilt`
#' @export
#'
Quilt <- setClass("Quilt",
                  slots=c(Label='character_list',
                          Description='character_list',
                          Value='array',
                          Color='character'
                  )
)

setMethod("initialize", "Quilt", function(.Object,
                                          Label=NULL,
                                          Description=NULL,
                                          Value=NULL,
                                          Color=c('#088ebc', 'white')) {
  .Object@Label <- use_ifnot_NULL('Label', Label, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Color <- use_ifnot_NULL('Color', Color, .Object)
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
                     Value=NULL,
                     Color=c('#088ebc', 'white')) {
  Quilt <- new('Quilt', Label, Description, Value, Color)
  validObject(Quilt)
  Quilt
}

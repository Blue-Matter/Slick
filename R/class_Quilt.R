# ---- Class Quilt ----
#' Create a new `Quilt` object
#'
#' @return An object of class `Quilt`
#' @export
#'
Quilt <- setClass("Quilt",
                  slots=c(Metadata='dataframe_list',
                          Value='array',
                          MinColor='character',
                          MaxColor='character'
                  )

)

# Metadata <- data.frame(Code, Label, Description, Default, MinValue, MaxValue)

setMethod("initialize", "Quilt", function(.Object,
                                          Metadata=NULL,
                                          Value=NULL,
                                          MinColor=NULL,
                                          MaxColor=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@MinColor <- use_ifnot_NULL('MinColor', MinColor, .Object)
  .Object@MaxColor <- use_ifnot_NULL('MaxColor', MaxColor, .Object)
  .Object
})


validQuilt <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Quilt', validQuilt)



newQuilt <- function(Metadata=NULL,
                     Value=NULL,
                     MinColor='#088ebc',
                     MaxColor='white') {
  Quilt <- new('Quilt', Metadata, Value, MinColor, MaxColor)
  validObject(Quilt)
  Quilt
}

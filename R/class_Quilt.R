# ---- Class Quilt ----
#' Create a new `Quilt` object
#'
#' @return An object of class `Quilt`
#' @export
#'
Quilt <- setClass("Quilt",
                  slots=c(Metadata='dataframe_list',
                          Value='array',
                          Preset='list',
                          MinColor='character',
                          MaxColor='character',
                          Tradeoff='list'
                  )

)


setMethod("initialize", "Quilt", function(.Object,
                                          Metadata=NULL,
                                          Value=NULL,
                                          Preset=NULL,
                                          MinColor=NULL,
                                          MaxColor=NULL,
                                          Tradeoff=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object@MinColor <- use_ifnot_NULL('MinColor', MinColor, .Object)
  .Object@MaxColor <- use_ifnot_NULL('MaxColor', MaxColor, .Object)
  .Object@Tradeoff <- use_ifnot_NULL('Tradeoff', Tradeoff, .Object)
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
                     Preset=NULL,
                     MinColor='#088ebc',
                     MaxColor='white',
                     Tradeoff=NULL) {
  Quilt <- new('Quilt', Metadata, Value, Preset, MinColor, MaxColor, Tradeoff)
  validObject(Quilt)
  Quilt
}

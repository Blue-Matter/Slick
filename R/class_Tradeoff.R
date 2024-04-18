# ---- Class Quilt ----
#' Create a new `Quilt` object
#'
#' @return An object of class `Quilt`
#' @export
#'
Tradeoff <- setClass("Tradeoff",
                  slots=c(Metadata='dataframe_list',
                          Value='array',
                          Preset='list',
                          Selected='character_numeric'
                  )
)

setMethod("initialize", "Tradeoff", function(.Object,
                                          Metadata=NULL,
                                          Value=NULL,
                                          Preset=NULL,
                                          Selected=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object@Selected <- use_ifnot_NULL('Selected', Selected, .Object)
  .Object
})


validTradeoff <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Tradeoff', validTradeoff)



newTradeoff <- function(Metadata=NULL,
                     Value=NULL,
                     Preset=NULL,
                     Selected=NULL) {
  Tradeoff <- new('Tradeoff', Metadata, Value, Preset, Selected)
  validObject(Tradeoff)
  Tradeoff
}

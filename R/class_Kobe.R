#' Create a new `Kobe` object
#'
#' @return An object of class `Kobe`
#' @export
#'
Kobe <- setClass("Kobe",
                 slots=c(Metadata='dataframe_list',
                         Time='dataframe_list',
                         Value='array',
                         Preset='list'
                 )
)

setMethod("initialize", "Kobe", function(.Object,
                                         Metadata=NULL,
                                         Time=NULL,
                                         Value=NULL,
                                         Preset=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object
})


validKobe <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Kobe', validKobe)


newKobe <- function(Metadata=NULL,
                    Time=NULL,
                    Value=NULL,
                    Preset=NULL) {
  Kobe <- new('Kobe',
              Metadata,
              Time, Value, Preset)
  validObject(Kobe)
  Kobe
}

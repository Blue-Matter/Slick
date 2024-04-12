#' Create a new `Kobe` object
#'
#' @return An object of class `Kobe`
#' @export
#'
Kobe <- setClass("Kobe",
                 slots=c(Metadata='dataframe_list',
                         Time='numeric',
                         TimeLab='character_list',
                         Value='array',
                         RefPoints='list',
                         RefNames='list',
                         Preset='list'
                 )
)

setMethod("initialize", "Kobe", function(.Object,
                                         Metadata=NULL,
                                         Time=NULL,
                                         TimeLab=NULL,
                                         Value=NULL,
                                         RefPoints=NULL,
                                         RefNames=NULL,
                                         Preset=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@TimeLab <- use_ifnot_NULL('TimeLab', TimeLab, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@RefPoints <- use_ifnot_NULL('RefPoints', RefPoints, .Object)
  .Object@RefNames <- use_ifnot_NULL('RefNames', RefNames, .Object)
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
                    TimeLab=NULL,
                    Value=NULL,
                    RefPoints=NULL,
                    RefNames=NULL,
                    Preset=NULL) {
  Kobe <- new('Kobe',
              Metadata,
              Time, TimeLab, Value, RefPoints, RefNames, Preset)
  validObject(Kobe)
  Kobe
}

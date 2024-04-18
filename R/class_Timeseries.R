#' Create a new `Timeseries` object
#'
#' @return An object of class `Timeseries`
#' @export
#'
Timeseries <- setClass("Timeseries",
                       slots=c(Metadata='dataframe_list',
                               Time='dataframe_list',
                               Value='array',
                               Preset='list'
                       )
)

setMethod("initialize", "Timeseries", function(.Object,
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

validTimeSeries <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Timeseries', validTimeSeries)


newTimeseries <- function(Metadata=NULL,
                          Time=NULL,
                          Value=NULL,
                          Preset=NULL) {
  Timeseries <- new('Timeseries',
                    Metadata,
                    Time,
                    Value,
                    Preset)

  validObject(Timeseries)
  Timeseries
}

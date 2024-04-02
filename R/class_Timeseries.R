#' Create a new `Timeseries` object
#'
#' @return An object of class `Timeseries`
#' @export
#'
Timeseries <- setClass("Timeseries",
                       slots=c(Metadata='dataframe_list',
                               Time='numeric',
                               TimeNow='numeric',
                               TimeLab='character_list',
                               Value='array',
                               RefPoints='list',
                               RefNames='list'
                       )
)

setMethod("initialize", "Timeseries", function(.Object,
                                               Metadata=NULL,
                                               Time=NULL,
                                               TimeNow=NULL,
                                               TimeLab=NULL,
                                               Value=NULL,
                                               RefPoints=NULL,
                                               RefNames=NULL) {

  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@TimeNow <- use_ifnot_NULL('TimeNow', TimeNow, .Object)
  .Object@TimeLab <- use_ifnot_NULL('TimeLab', TimeLab, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@RefPoints <- use_ifnot_NULL('RefPoints', RefPoints, .Object)
  .Object@RefNames <- use_ifnot_NULL('RefNames', RefNames, .Object)
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
                          TimeNow=NULL,
                          TimeLab=NULL,
                          Value=NULL,
                          RefPoints=NULL,
                          RefNames=NULL) {
  Timeseries <- new('Timeseries',
                    Metadata,
                    Time,
                    TimeNow,
                    TimeLab,
                    Value,
                    RefPoints,
                    RefNames)
  validObject(Timeseries)
  Timeseries
}

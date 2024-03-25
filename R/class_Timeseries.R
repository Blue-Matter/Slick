#' Create a new `Timeseries` object
#'
#' @return An object of class `Timeseries`
#' @export
#'
Timeseries <- setClass("Timeseries",
                       slots=c(Label='character_list',
                               Description='character_list',
                               Time='numeric',
                               TimeNow='numeric',
                               TimeLab='character_list',
                               Value='list',
                               RefPoints='list',
                               RefNames='list'
                       )
)

setMethod("initialize", "Timeseries", function(.Object,
                                               Label=NULL,
                                               Description=NULL,
                                               Time=NULL,
                                               TimeNow=NULL,
                                               TimeLab=NULL,
                                               Value=NULL,
                                               RefPoints=NULL,
                                               RefNames=NULL) {
  .Object@Label <- use_ifnot_NULL('Label', Label, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@TimeNow <- use_ifnot_NULL('TimeNow', TimeNow, .Object)
  .Object@TimeLab <- use_ifnot_NULL('TimeLab', Description, .Object)
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


newTimeSeries <- function(Label=NULL,
                          Description=NULL,
                          Time=NULL,
                          TimeLab=NULL,
                          Value=NULL,
                          RefPoints=NULL,
                          RefNames=NULL) {
  Timeseries <- new('Timeseries', Label, Description, Value,
                    Time, TimeLab, TimeNow, Value, RefPoints, RefNames)
  validObject(Timeseries)
  Timeseries
}

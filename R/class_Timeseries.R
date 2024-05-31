#' `Timeseries` S4 class and functions
#'
#' An object of class `Timeseries` contains information for the Time Series chart.
#' The `Timeseries` function is used both to create and modify an object of class [Timeseries]
#' and to access and assign the `Timeseries` slot in an object of class [Slick]. See `Details`.
#'
#' @details
#' ## Creating objects of class [Timeseries]
#' - `Timeseries()` creates a new object of class [Timeseries]
#' - `Timeseries(object)` assigns the [Metadata] slot in an object of class [Timeseries],
#' where `object` is either a `data.frame` or a named `list`.  See `Metadata` section below.
#'
#' ## Accessing and assigning `Timeseries` slot in objects of class [Slick]
#' TODO
#'
#' ## Metadata
#' TODO
#'
#' ## Time
#' TODO
#'
#' ## Value
#' TODO
#'
#' ## Preset
#' TODO
#'
#' @slot Metadata TODO
#' @slot Time TODO
#' @slot Value TODO
#' @slot Preset TODO
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

  methods::validObject(Timeseries)
  Timeseries
}

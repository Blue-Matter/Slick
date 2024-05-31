#' `Tradeoff` S4 class and functions
#'
#' An object of class `Tradeoff` contains information for the Time Series chart.
#' The `Tradeoff` function is used both to create and modify an object of class [Tradeoff]
#' and to access and assign the `Tradeoff` slot in an object of class [Slick]. See `Details`.
#'
#' @details
#' ## Creating objects of class [Tradeoff]
#' - `Tradeoff()` creates a new object of class [Tradeoff]
#'
#' ## Accessing and assigning `Tradeoff` slot in objects of class [Slick]
#' TODO
#'
#' ## Metadata
#' TODO

#' ## Value
#' TODO
#'
#' ## Preset
#' TODO
#'
#' ## Selected
#'
#' @slot Metadata TODO
#' @slot Value TODO
#' @slot Preset TODO
#' @slot Selected TODO
#'
#' @return An object of class `Tradeoff`
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
  methods::validObject(Tradeoff)
  Tradeoff
}

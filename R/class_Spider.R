#' `Spider` S4 class and functions
#'
#' An object of class `Spider` contains information for the Spider chart.
#' The `Spider` function is used both to create and modify an object of class [Spider]
#' and to access and assign the `Spider` slot in an object of class [Slick]. See `Details`.
#'
#' @details
#' TODO
#'
#' ## Metadata
#' TODO
#'
#' ## Value
#' TODO
#'
#' ## Preset
#' TODO
#'
#'
#' @slot Metadata TODO
#' @slot Value TODO
#' @slot Preset TODO
#'
#' @return An object of class `Spider`
#' @export
#'
Spider <- setClass("Spider",
                   slots=c(Metadata='dataframe_list',
                           Value='array',
                           Preset='list'
                   )
)

setMethod("initialize", "Spider", function(.Object,
                                           Metadata=NULL,
                                           Value=NULL,
                                           Preset=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object
})


validSpider <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Spider', validSpider)


newSpider <- function(Metadata=NULL,
                      Value=NULL,
                      Preset=NULL) {
  Spider <- new('Spider', Metadata, Value, Preset)
  methods::validObject(Spider)
  Spider
}

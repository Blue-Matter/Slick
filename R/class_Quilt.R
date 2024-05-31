

#' `Quilt` S4 class and functions
#'
#' An object of class `Quilt` contains information for the Quilt chart.
#' The `Quilt` function is used both to create and modify an object of class [Quilt]
#' and to access and assign the `Quilt` slot in an object of class [Slick]. See `Details`.
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
#' ## Colors
#' TODO
#'
#' @slot Metadata TODO
#' @slot Value TODO
#' @slot Preset TODO
#' @slot Color TODO
#'
#' @return An object of class `Quilt`
#' @export
#'
Quilt <- setClass("Quilt",
                  slots=c(Metadata='dataframe_list',
                          Value='array',
                          Preset='list',
                          Colors='character'
                  )

)


setMethod("initialize", "Quilt", function(.Object,
                                          Metadata=NULL,
                                          Value=NULL,
                                          Preset=NULL,
                                          Colors=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object@Colors <- use_ifnot_NULL('Colors', Colors, .Object)
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
                     Colors=c('#0AFF12', '#FC0828')) {
  Quilt <- new('Quilt', Metadata, Value, Preset, Colors)
  methods::validObject(Quilt)
  Quilt
}

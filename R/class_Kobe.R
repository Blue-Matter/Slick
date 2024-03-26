#' Create a new `Kobe` object
#'
#' @return An object of class `Kobe`
#' @export
#'
Kobe <- setClass("Kobe",
                 slots=c(Label='character_list',
                         Description='character_list',
                         Time='numeric',
                         TimeLab='character_list',
                         Value='array',
                         RefPoints='list',
                         RefNames='list'
                 )
)

setMethod("initialize", "Kobe", function(.Object,
                                         Label=NULL,
                                         Description=NULL,
                                         Time=NULL,
                                         TimeLab=NULL,
                                         Value=NULL,
                                         RefPoints=NULL,
                                         RefNames=NULL) {
  .Object@Label <- use_ifnot_NULL('Label', Label, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@TimeLab <- use_ifnot_NULL('TimeLab', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@RefPoints <- use_ifnot_NULL('RefPoints', RefPoints, .Object)
  .Object@RefNames <- use_ifnot_NULL('RefNames', RefNames, .Object)
  .Object
})


validKobe <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Kobe', validKobe)


newKobe <- function(Label=NULL,
                    Description=NULL,
                    Time=NULL,
                    TimeLab=NULL,
                    Value=NULL,
                    RefPoints=NULL,
                    RefNames=NULL) {
  Kobe <- new('Kobe', Label, Description,
              Time, TimeLab, Value, RefPoints, RefNames)
  validObject(Kobe)
  Kobe
}

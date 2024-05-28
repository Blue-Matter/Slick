# ---- Class Boxplot ----
#' Create a new `Boxplot` object
#'
#' @return An object of class `Boxplot`
#' @export
Boxplot <- setClass("Boxplot",
                    slots=c(Metadata='dataframe_list',
                            Value='array',
                            Preset='list'
                    )
)

setMethod("initialize", "Boxplot", function(.Object,
                                            Metadata=NULL,
                                            Value=NULL,
                                            Preset=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object
})


validBoxplot <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Boxplot', validBoxplot)

newBoxplot <- function(Metadata=NULL,
                      Value=NULL,
                      Preset=NULL) {
  Boxplot <- new('Boxplot', Metadata, Value, Preset)
  methods::validObject(Boxplot)
  Boxplot
}

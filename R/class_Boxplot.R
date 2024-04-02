# ---- Class Boxplot ----
#' Create a new `Boxplot` object
#'
#' @return An object of class `Boxplot`
#' @export
Boxplot <- setClass("Boxplot",
                    slots=c(Metadata='dataframe_list',
                            Value='array'
                    )
)

setMethod("initialize", "Boxplot", function(.Object,
                                            Metadata=NULL,
                                            Value=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
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
                      Value=NULL) {
  Boxplot <- new('Boxplot', Metadata, Value)
  validObject(Boxplot)
  Boxplot
}

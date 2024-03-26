# ---- Class Boxplot ----
#' Create a new `Boxplot` object
#'
#' @return An object of class `Boxplot`
#' @export
Boxplot <- setClass("Boxplot",
                    slots=c(Label='character_list',
                            Description='character_list',
                            Value='array'
                    )
)

setMethod("initialize", "Boxplot", function(.Object,
                                            Label=NULL,
                                            Description=NULL,
                                            Value=NULL) {
  .Object@Label <- use_ifnot_NULL('Label', Label, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
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

newBoxplot <- function(Label=NULL,
                      Description=NULL,
                      Value=NULL) {
  Boxplot <- new('Boxplot', Label, Description, Value)
  validObject(Boxplot)
  Boxplot
}

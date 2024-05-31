
# ---- Class ----

#' S4 class Boxplot
#'
#' Slots can be accessed and assigned using functions corresponding to slot name.
#' See `See Also` section below.
#'
#' @details
#' Objects of class `Boxplot` are created with `Boxplot()`
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of performance indicators (`nPIs`) in `Value`
#'
#' @slot Code `r code_PI_param()`
#' @slot Label `r label_PI_param() `
#' @slot Description `r description_PI_param()`
#' @slot Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, and `nPI`)
#' @slot Preset `r preset_param()`
#'
#' @seealso [Boxplot-methods()], [Code()], [Label()], [Description()],
#'  [Metadata()], [Value()], [Preset()]
#'
#' @example inst/examples/Boxplot.R
#' @docType class
#' @export
setClass("Boxplot",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Value='array',
                 Preset='list'
         )
)

## ---- Initialize ----
setMethod("initialize", "Boxplot", function(.Object,
                                            Code='',
                                            Label='',
                                            Description='',
                                            Value=array(),
                                            Preset=list()) {
  .Object@Code <- use_ifnot_NULL('Code', Code, .Object)
  .Object@Label <- use_ifnot_NULL('Label', Label, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object
})

## ---- Validate ----
validBoxplot <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Boxplot', validBoxplot)

## ---- New ----
newBoxplot <- function(Code='',
                       Label='',
                       Description='',
                       Value=array(),
                       Preset=list()) {
  Boxplot <- new('Boxplot', Code, Label, Description, Value, Preset)
  methods::validObject(Boxplot)
  Boxplot
}



# ---- Methods ----


#' Methods for Creating, Accessing and Assigning `Boxplot` objects
#'
#' An object of class `Boxplot` contains information for the Boxplot chart.
#' The `Boxplot` function is used both to create and modify an [Boxplot-class()] object.
#' and to access and assign `Boxplot` for an object of class [Slick-class()].
#' See `Details`.
#'
#' @param Code `r code_PI_param()`
#' @param Label  `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Value  A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, and `nPI`)
#' @param Preset `r preset_param()`
#'
#' @details
#' - `Boxplot()`: creates an empty `Boxplot` object
#' - `Boxplot(Code, ...)`: creates a populated `Boxplot` object
#' - `Boxplot(Slick)`: returns the `Boxplot` from a `Slick` object
#' - `Boxplot(Slick) <- Boxplot`: assigns a `Boxplot` object to a `Slick` object
#'
#' Use the  [Code()], [Label()], [Description()], [Value()], [Preset()] functions to access and assign the values for an
#' existing `Boxplot` object, see `Examples`
#'
#' @rdname Boxplot-methods
#' @docType methods
#' @example inst/examples/Boxplot.R
#' @seealso [Code()], [Label()], [Description()], [Metadata()], [Value()], [Preset()]
#' @export
setGeneric("Boxplot", function(Code,
                               Label,
                               Description,
                               Value,
                               Preset) standardGeneric("Boxplot"))


setMethod("Boxplot", signature(Code="missing",
                               Label='missing',
                               Description='missing',
                               Value='missing',
                               Preset='missing'), function() newBoxplot())

setMethod("Boxplot", c("character_list", 'missing','missing','missing', 'missing'),
          function(Code) newBoxplot(Code))

setMethod("Boxplot", c("character_list", 'character_list','missing','missing', 'missing'),
          function(Code, Label) newBoxplot(Code, Label))

setMethod("Boxplot", c("character_list", 'character_list','character_list','missing', 'missing'),
          function(Code, Label, Description) newBoxplot(Code, Label, Description))

setMethod("Boxplot", c("character_list", 'character_list','character_list','array', 'missing'),
          function(Code, Label, Description, Value) newBoxplot(Code, Label, Description, Value))

setMethod("Boxplot", c("character_list", 'character_list','character_list','array', 'list'),
          function(Code, Label, Description, Value, Preset) newBoxplot(Code, Label, Description, Value, Preset))


setMethod("Boxplot", c("missing", 'character_list','missing','missing', 'missing'),
          function(Label) newBoxplot(Label=Label))

setMethod("Boxplot", c("missing", 'character_list','character_list','missing', 'missing'),
          function(Label, Description) newBoxplot(Label=Label, Description=Description))

setMethod("Boxplot", c("missing", 'character_list','character_list','array', 'missing'),
          function(Label, Description, Value) newBoxplot(Label=Label, Description=Description, Value=Value))

setMethod("Boxplot", c("missing", 'character_list','character_list','array', 'list'),
          function(Label, Description, Value, Preset) newBoxplot(Label=Label, Description=Description, Value=Value, Preset=Preset))



setMethod("Boxplot", c("missing", 'missing','character_list','missing', 'missing'),
          function(Description) newBoxplot(Description=Description))

setMethod("Boxplot", c("missing", 'missing','character_list','array', 'missing'),
          function(Description, Value) newBoxplot(Description=Description, Value=Value))

setMethod("Boxplot", c("missing", 'missing','character_list','array', 'list'),
          function(Description, Value, Preset) newBoxplot(Description=Description, Value=Value, Preset=Preset))



setMethod("Boxplot", c("missing", 'missing','missing','array', 'missing'),
          function(Value) newBoxplot(Value=Value))

setMethod("Boxplot", c('missing', 'missing','missing','missing', 'list'),
          function(Preset) newBoxplot(Preset=Preset))



## --- Show ----

#' @rdname Boxplot-class
#' @param object An object of class [Boxplot-class()]
#' @export
setMethod("show", "Boxplot", function(object) {
  cat('An object of class `Boxplot` \n\n')

  cat('Performance Indicators: \n')
  metadata <- Metadata(object)

  if (all(apply(metadata,1, nchar)[,1]==0)) {
    cat('Not populated\n')
    print(metadata)
  } else {
    print(metadata)
  }

  cat('\nPerformance Indicators:\n')
  if (all(is.na(object@Value))) {
    cat('Not populated\n')
  } else {
    dd <- dim(object@Value)
    dims <- c('nsim', 'nOM', 'nMP', 'nPI')
    txt <- rep(NA, length(dd))
    for (i in seq_along(dd)) {
      txt[i] <- paste0(dims[i], ':', dd[i])
    }
    cat(txt)
    cat('\n')
    if (length(dd) < length(dims)) {
      cat('\nWARNING: too few dimensions in `Value`')
    }
    if (length(dd) > length(dims)) {
      cat('\nWARNING: too many dimensions in `Value`')
    }

  }

  if (!(all(apply(metadata,1, nchar)[,1]==0)) && (length(dd) == length(dims)) & !all(is.na(object@Value))) {
    if (nrow(metadata) != dd[4])
      cat('\nWARNING:\n The PI dimension in `Value` (dimension 4) does not match the number of performance indicators described in `Code`, `Label`, and `Description`\n')
  }


  preset <- object@Preset
  cat('\nPresets: \n')
  if (length(preset)<1) {
    cat('None')
  } else {
    print(object@Preset)
  }


})



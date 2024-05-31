# ---- Class ----

#' S4 class Kobe
#'
#' Slots can be accessed and assigned using functions corresponding to slot name.
#' See `See Also` section below.
#'
#' @details
#' Objects of class `Kobe` are created with `Kobe()`
#'
#' ## Performance Indicators
#' There must be exactly two performance indicators (PIs).
#' The first PI will be on the x-axis (usually B/BMSY or something similar) and the second
#' on the y-axis (e.g., F/FMSY)
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
#'
#' @slot Code `r code_PI_param()`
#' @slot Label `r label_PI_param() `
#' @slot Description `r description_PI_param()`
#' @slot Time A data.frame with a single column of values for the projection time-steps.
#' The column name will be used as the label in the `Kobe Time` plot. Use a named list for
#' multiple languages.
#' @slot Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), performance indicator (PI),
#' and projection time-steps (nTS)
#' Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)
#' @slot Preset `r preset_param()`
#'
#' @seealso [Kobe-methods()], [Code()], [Label()], [Description()],
#'  [Metadata()], [Value()], [Preset()]
#'
#' @example inst/examples/Kobe.R
#' @docType class
#' @export

setClass("Kobe",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Time='dataframe_list',
                 Value='array',
                 Preset='list'
         )
)

## ---- Initialize ----
setMethod("initialize", "Kobe", function(.Object,
                                         Code=NULL,
                                         Label=NULL,
                                         Description=NULL,
                                         Time=NULL,
                                         Value=NULL,
                                         Preset=NULL) {
  .Object@Metadata <- use_ifnot_NULL('Metadata', Metadata, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@Preset <- use_ifnot_NULL('Preset', Preset, .Object)
  .Object
})

## ---- Validate ----
validKobe <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Kobe', validKobe)

## ---- New ----
newKobe <- function(Code='',
                    Label='',
                    Description='',
                    Time=data.frame(),
                    Value=array(),
                    Preset=list()) {
  Kobe <- new('Kobe',
              Code,
              Label,
              Description,
              Time,
              Value,
              Preset)
  methods::validObject(Kobe)
  Kobe
}


# ---- Methods ----


#' Methods for Creating, Accessing and Assigning `Kobe` objects
#'
#' An object of class `Kobe` contains information for the Kobe chart.
#' The `Kobe` function is used both to create and modify an [Kobe-class()] object.
#' and to access and assign `Kobe` for an object of class [Slick-class()].
#' See `Details`.
#'
#' The Kobe plot typically shows B/BMSY (or something similar) on the x-axis, and
#' F/FMSY (or something similar) on the y-axis.
#'
#' ## Performance Indicators
#' There must be exactly two performance indicators (PIs).
#' The first PI will be on the x-axis (usually B/BMSY or something similar) and the second
#' on the y-axis (e.g., F/FMSY)
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
#' @param Code `r code_PI_param()`
#' @param Label  `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Time A data.frame with a single column of values for the projection time-steps.
#' The column name will be used as the label in the `Kobe Time` plot. Use a named list for
#' multiple languages.
#' @param Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), performance indicator (PI),
#' and projection time-steps (nTS).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)
#' @param Preset `r preset_param()`
#'
#' @details
#' - `Kobe()`: creates an empty `Kobe` object
#' - `Kobe(Code, ...)`: creates a populated `Kobe` object
#' - `Kobe(Slick)`: returns the `Kobe` from a `Slick` object
#' - `Kobe(Slick) <- Kobe`: assigns a `Kobe` object to a `Slick` object
#'
#' Use the  [Code()], [Label()], [Description()], [Value()], [Preset()] functions to access and assign the values for an
#' existing `Kobe` object, see `Examples`
#'
#' @rdname Kobe-methods
#' @docType methods
#' @example inst/examples/Kobe.R
#' @seealso [Code()], [Label()], [Description()], [Metadata()], [Value()], [Preset()]
#' @export
setGeneric("Kobe", function(Code,
                            Label,
                            Description,
                            Time,
                            Value,
                            Preset) standardGeneric("Kobe"))

setMethod("Kobe", signature(Code="missing",
                            Label='missing',
                            Description='missing',
                            Time='missing',
                            Value='missing',
                            Preset='missing'), function() newKobe())

setMethod("Kobe", signature(Code="character_list",
                            Label='character_list',
                            Description='character_list',
                            Time='dataframe_list',
                            Value='array',
                            Preset='missing'),
          function(Code, Label, Description, Time, Value) newKobe(Code, Label, Description, Time, Value))

setMethod("Kobe", signature(Code="character_list",
                            Label='character_list',
                            Description='character_list',
                            Time='dataframe_list',
                            Value='array',
                            Preset='list'),
          function(Code, Label, Description, Time, Value, Preset) newKobe(Code, Label, Description, Time, Value,Preset))


## --- Show ----

#' @rdname Kobe-class
#' @param object An object of class [Kobe-class()]
#' @export
setMethod("show", "Kobe", function(object) {
  cat('An object of class `Kobe` \n\n')



  # cat('Performance Indicators: \n')
  # metadata <- Metadata(object)
  #
  # if (all(apply(metadata,1, nchar)[,1]==0)) {
  #   cat('Not populated\n')
  #   print(metadata)
  # } else {
  #   print(metadata)
  # }
  #
  # cat('\nPerformance Indicators:\n')
  # if (all(is.na(object@Value))) {
  #   cat('Not populated\n')
  # } else {
  #   dd <- dim(object@Value)
  #   dims <- c('nsim', 'nOM', 'nMP', 'nPI')
  #   txt <- rep(NA, length(dd))
  #   for (i in seq_along(dd)) {
  #     txt[i] <- paste0(dims[i], ':', dd[i])
  #   }
  #   cat(txt)
  #   cat('\n')
  #   if (length(dd) < length(dims)) {
  #     cat('\nWARNING: too few dimensions in `Value`')
  #   }
  #   if (length(dd) > length(dims)) {
  #     cat('\nWARNING: too many dimensions in `Value`')
  #   }
  #
  # }
  #
  # if (!(all(apply(metadata,1, nchar)[,1]==0)) && (length(dd) == length(dims)) & !all(is.na(object@Value))) {
  #   if (nrow(metadata) != dd[4])
  #     cat('\nWARNING:\n The PI dimension in `Value` (dimension 4) does not match the number of performance indicators described in `Code`, `Label`, and `Description`\n')
  # }
  #
  #
  # preset <- object@Preset
  # cat('\nPresets: \n')
  # if (length(preset)<1) {
  #   cat('None')
  # } else {
  #   print(object@Preset)
  # }


})

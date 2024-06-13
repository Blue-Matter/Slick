
# ---- Class ----

#' S4 class `Timeseries`
#'
#' Objects of class `Timeseries` are used to store information for the Time Series plots.
#' Like all S4 objects in `Slick`, slots in this object can be accessed and
#' assigned using functions corresponding to slot name. See [Timeseries()] and the
#' the `See Also` section below.
#'
#' @details
#' Objects of class `Timeseries` are created with `Timeseries()`
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
#' @slot Time A numeric vector with values for the historical and projection time-steps. Must
#' match length `nTS` in `Value`
#' @slot TimeNow A numeric value matching the last historical timestep in `Time`
#' @slot TimeLab Character string length 1. Name of the time step (e.g., 'Year'). Will be used as the label in the plots.
#' Use a named list for multiple languages.
#' @slot Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP),  performance indicator (PI),
#' and historical + projection timestep (nTS).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)
#' @slot Preset `r preset_param()`
#' @slot Target Numeric vector length `nPI` with the target value for the PIs.
#' @slot Limit Numeric vector length `nPI` with the limit value for the PIs.
#'
#' @seealso [Timeseries-methods()], [Code()], [Label()], [Description()],
#'  [Value()], [Preset()]
#'
#' @example inst/examples/Timeseries.R
#' @docType class
#' @export
setClass("Timeseries",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Time='numeric',
                 TimeNow='numeric',
                 TimeLab='character_list',
                 Value='array',
                 Preset='list',
                 Target='numericOrNULL',
                 Limit='numericOrNULL'
         )
)

setMethod("initialize", "Timeseries", function(.Object,
                                            Code='',
                                            Label='',
                                            Description='',
                                            Time=numeric(),
                                            TimeNow=numeric(),
                                            TimeLab='Year',
                                            Value=array(),
                                            Preset=list(),
                                            Target=NULL,
                                            Limit=NULL) {
  .Object@Code <- Code
  .Object@Label <- Label
  .Object@Description <- Description
  .Object@Time <- Time
  .Object@TimeNow <- TimeNow
  .Object@TimeLab <- TimeLab
  .Object@Description <- Description
  .Object@Value <- Value
  .Object@Preset <- Preset
  .Object@Target <- Target
  .Object@Limit <- Limit
  methods::validObject(.Object)
  .Object
})


validTimeSeries <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Timeseries', validTimeSeries)

# ---- Methods ----

#' @describeIn Timeseries-methods Create an empty `Timeseries` object
setMethod("Timeseries", 'missing', function() new('Timeseries'))

#' @describeIn Timeseries-methods Create a populated `Timeseries` object
setMethod("Timeseries", c('character'),
          function(Code, Label, Description, Time, TimeNow, TimeLab,
                   Value, Preset, Target, Limit)
            new('Timeseries', Code, Label, Description, Time, TimeNow, TimeLab,
                Value, Preset, Target, Limit))

#' @describeIn Timeseries-methods Create a populated `Timeseries` object
setMethod("Timeseries", c('list'),
          function(Code, Label, Description, Time, TimeNow, TimeLab,
                   Value, Preset, Target, Limit)
            new('Timeseries', Code, Label, Description, Time, TimeNow, TimeLab,
                Value, Preset, Target, Limit))


## Check ----
#' @describeIn Check Check [Timeseries-class()] objects for errors
setMethod('Check', 'Timeseries', function(object) {

  ll <- CheckList()
  ll@object <- class(object)

  ll@empty <- is_empty(object)
  if (ll@empty) return(ll)
  ll@empty <- FALSE


  ll
})

## Code ----

#' @describeIn Code Return `Code` from a [Timeseries-class()] object
setMethod("Code", 'Timeseries', function(object, lang='en') {
  get_language(object@Code, lang)
})


#' @describeIn Code Assign `Code` to a [Timeseries-class()] object
setMethod("Code<-", 'Timeseries', function(object, value) {
  object@Code <- value
  methods::validObject(object)
  object
})

## Label ----

#' @describeIn Code Return `Label` from a [Timeseries-class()] object
setMethod("Label", 'Timeseries', function(object, lang='en') {
  get_language(object@Label, lang)
})


#' @describeIn Code Assign `Label` to a [Timeseries-class()] object
setMethod("Label<-", 'Timeseries', function(object, value) {
  object@Label <- value
  methods::validObject(object)
  object
})

## Description ----

#' @describeIn Code Return `Description` from a [Timeseries-class()] object
setMethod("Description", 'Timeseries', function(object, lang='en') {
  get_language(object@Description, lang)
})


#' @describeIn Code Assign `Description` to a [Timeseries-class()] object
setMethod("Description<-", 'Timeseries', function(object, value) {
  object@Description <- value
  methods::validObject(object)
  object
})

## Value ----
#' @describeIn Value  Return `Value` from a [Timeseries-class()] object
setMethod("Value", 'Timeseries', function(object) {
  object@Value
})

#' @describeIn Value Assign `Value` to a [Timeseries-class()] object
setMethod("Value<-", "Timeseries", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

## Preset ----

#' @describeIn Preset Return `Preset` from a [Timeseries-class()] object
setMethod("Preset", 'Timeseries', function(object) {
  object@Preset
})

#' @describeIn Preset Assign `Preset` to a [Timeseries-class()] object
setMethod("Preset<-", "Timeseries", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


## Time ----
#' @describeIn Time Return `Time` from a [Timeseries-class()] object
setMethod("Time", "Timeseries", function(object) {object@Time})

#' @describeIn Time Assign `Time` to a [Timeseries-class()] object
setMethod("Time<-", "Timeseries", function(object, value) {
  object@Time <- value
  methods::validObject(object)
  object
})

## TimeNow ----
#' Access and return `TimeNow` from a [Timeseries-class()] object
#' @param Timeseries  A [Timeseries-class()] object
#' @param value A character string label for the time axis. Use a named list for
#' multiple languages
#'
#' @export
TimeNow <- function(Timeseries) {
  Timeseries@TimeNow
}

#' @describeIn TimeNow Assign `TimeNow` to a [Timeseries-class()] object
#'
#' @export
`TimeNow<-` <- function(Timeseries, value) {
  Timeseries@TimeNow <- value
  methods::validObject(Timeseries)
  Timeseries
}


## TimeLab ----
#' @describeIn TimeLab Return `TimeLab` from a [Timeseries-class()] object
setMethod("TimeLab", "Timeseries", function(object, lang='es') {
  get_language(object@TimeLab, lang)
})


#' @describeIn TimeLab Assign `TimeLab` to a [Timeseries-class()] object
setMethod("TimeLab<-", "Timeseries", function(object, value) {
  object@TimeLab <- value
  methods::validObject(object)
  object
})



## Target ----

#' @describeIn Target Return `Target` from a [Timeseries-class()] object
setMethod("Target", "Timeseries", function(object) {
  object@Target
})


#' @describeIn Target Assign `Target` to a [Timeseries-class()] object
setMethod("Target<-", "Timeseries", function(object, value) {
  object@Target <- value
  methods::validObject(object)
  object
})

## Limit ----

#' @describeIn Target Return `Limit` from a [Timeseries-class()] object
setMethod("Limit", "Timeseries", function(object) {
  object@Limit
})


#' @describeIn Target Assign `Limit` to a [Timeseries-class()] object
setMethod("Limit<-", "Timeseries", function(object, value) {
  object@Limit <- value
  methods::validObject(object)
  object
})

## Metadata ----

#' @describeIn Metadata Return Metadata for [Timeseries-class()] objects
#' @export
setMethod('Metadata', 'Timeseries', function(object, lang='en') {
  data.frame(Code=object@Code,
             Label=get_language(object@Label, lang),
             Description=get_language(object@Description, lang))

})

#' @describeIn Metadata Assign Metadata for [Timeseries-class()] objects
setMethod("Metadata<-", "Timeseries", function(object, value) {

  names <- c('Code', 'Label', 'Description')
  object <- check_assign_dataframe(object, names, value)
  methods::validObject(object)
  object
})


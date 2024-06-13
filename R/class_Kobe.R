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
#' @slot Time A numeric vector with values for the projection time-steps. Must
#' match length `nTS` in `Value`
#' @slot TimeLab Character string length 1. Name of the time step (e.g., 'Year'). Will be used as the label in the `Kobe Time` plot. Use a named list for
#' multiple languages.
#' @slot Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), performance indicator (PI),
#' and projection time-steps (nTS)
#' Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)
#' @slot Preset `r preset_param()`
#' @slot Target Numeric vector length 2 with the target value for the two PIs. Defines the color quadrants on the Kobe plot. Defaults to c(1,1).
#' @slot Limit Numeric vector length 2 with the limit value for the two PIs. Shows as red line on Kobe plot. NULL to ignore.
#'
#' @seealso [Kobe-methods()], [Code()], [Label()], [Description()],
#' [Value()], [Preset()]
#'
#' @example inst/examples/Kobe.R
#' @docType class
#' @export

setClass("Kobe",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Time='numeric',
                 TimeLab='character_list',
                 Value='array',
                 Preset='list',
                 Target='numericOrNULL',
                 Limit='numericOrNULL'
         )
)

## ---- Initialize ----
setMethod("initialize", "Kobe", function(.Object,
                                         Code='',
                                         Label='',
                                         Description='',
                                         Time=numeric(),
                                         TimeLab='Year',
                                         Value=array(),
                                         Preset=list(),
                                         Target=1,
                                         Limit=NULL) {
  .Object@Code <- Code
  .Object@Label <- Label
  .Object@Description <- Description
  .Object@Time <- Time
  .Object@TimeLab <- TimeLab
  .Object@Value <- Value
  .Object@Preset <- Preset
  .Object@Target <- Target
  .Object@Limit <- Limit
  methods::validObject(.Object)
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


# ---- Methods ----

#' @describeIn Kobe-methods Create an empty `Kobe` object
setMethod("Kobe", 'missing', function() new('Kobe'))

#' @describeIn Kobe-methods Create a populated `Kobe` object
setMethod("Kobe", c('character'),
          function(Code, Label, Description, Time, TimeLab, Value, Preset, Target, Limit)
            new('Kobe', Code, Label, Description, Time, TimeLab, Value, Preset, Target, Limit))

#' @describeIn Kobe-methods Create a populated `Kobe` object
setMethod("Kobe", c('list'),
          function(Code, Label, Description, Time, TimeLab, Value, Preset, Target, Limit)
            new('Kobe', Code, Label, Description, Time, TimeLab, Value, Preset, Target, Limit))



## Code ----

#' @describeIn Code Return `Code` from a [Kobe-class()] object
setMethod("Code", 'Kobe', function(object, lang='en') {
  get_language(object@Code, lang)
})


#' @describeIn Code Assign `Code` to a [Kobe-class()] object
setMethod("Code<-", 'Kobe', function(object, value) {
  object@Code <- value
  methods::validObject(object)
  object
})

## Label ----

#' @describeIn Code Return `Label` from a [Kobe-class()] object
setMethod("Label", 'Kobe', function(object, lang='en') {
  get_language(object@Label, lang)
})


#' @describeIn Code Assign `Label` to a [Kobe-class()] object
setMethod("Label<-", 'Kobe', function(object, value) {
  object@Label <- value
  methods::validObject(object)
  object
})

## Description ----

#' @describeIn Code Return `Description` from a [Kobe-class()] object
setMethod("Description", 'Kobe', function(object, lang='en') {
  get_language(object@Description, lang)
})


#' @describeIn Code Assign `Description` to a [Kobe-class()] object
setMethod("Description<-", 'Kobe', function(object, value) {
  object@Description <- value
  methods::validObject(object)
  object
})

## Value ----
#' @describeIn Value  Return `Value` from a [Kobe-class()] object
setMethod("Value", 'Kobe', function(object) {
  object@Value
})

#' @describeIn Value Assign `Value` to a [Kobe-class()] object
setMethod("Value<-", "Kobe", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

## Preset ----

#' @describeIn Preset Return `Preset` from a [Kobe-class()] object
setMethod("Preset", 'Kobe', function(object) {
  object@Preset
})

#' @describeIn Preset Assign `Preset` to a [Kobe-class()] object
setMethod("Preset<-", "Kobe", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


## Time ----
#' @describeIn Time Return `Time` from a [Kobe-class()] object
setMethod("Time", "Kobe", function(object) {object@Time})

#' @describeIn Time Assign `Time` to a [Kobe-class()] object
setMethod("Time<-", "Kobe", function(object, value) {
  object@Time <- value
  methods::validObject(object)
  object
})


## TimeLab ----
#' @describeIn TimeLab Return `TimeLab` from a [Kobe-class()] object
setMethod("TimeLab", "Kobe", function(object, lang='es') {
  get_language(object@TimeLab, lang)
})


#' @describeIn TimeLab Assign `TimeLab` to a [Kobe-class()] object
setMethod("TimeLab<-", "Kobe", function(object, value) {
  object@TimeLab <- value
  methods::validObject(object)
  object
})



## Target ----

#' @describeIn Target Return `Target` from a [Kobe-class()] object
setMethod("Target", "Kobe", function(object) {
  object@Target
})


#' @describeIn Target Assign `Target` to a [Kobe-class()] object
setMethod("Target<-", "Kobe", function(object, value) {
  object@Target <- value
  methods::validObject(object)
  object
})

## Limit ----

#' @describeIn Target Return `Limit` from a [Kobe-class()] object
setMethod("Limit", "Kobe", function(object) {
  object@Limit
})


#' @describeIn Target Assign `Limit` to a [Kobe-class()] object
setMethod("Limit<-", "Kobe", function(object, value) {
  object@Limit <- value
  methods::validObject(object)
  object
})



##  Show ----


## Metadata ----


#' @describeIn Metadata Return Metadata for [Kobe-class()] objects
#' @export
setMethod('Metadata', 'Kobe', function(object, lang='en') {
  data.frame(Code=object@Code,
             Label=get_language(object@Label, lang),
             Description=get_language(object@Description, lang))

})


#' @describeIn Metadata Assign Metadata for [Kobe-class()] objects
setMethod("Metadata<-", "Kobe", function(object, value) {

  names <- c('Code', 'Label', 'Description')
  object <- check_assign_dataframe(object, names, value)
  methods::validObject(object)
  object
})

# Check ----
#' @describeIn Check Check [Kobe-class()] objects for errors
setMethod('Check', 'Kobe', function(object) {

  ll <- CheckList()
  ll@object <- class(object)

  ll@empty <- is_empty(object)
  if (ll@empty) return(ll)
  ll@empty <- FALSE


  ll
})

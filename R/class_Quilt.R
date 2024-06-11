
# ---- Class ----

#' S4 class `Quilt`
#'
#' Objects of class `Quilt` are used to store information for the Quilt chart.
#' Like all S4 objects in `Slick`, slots in this object can be accessed and
#' assigned using functions corresponding to slot name. See [Quilt()] and the
#' the `See Also` section below.
#'
#' @details
#' Objects of class `Quilt` are created with `Quilt()`
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
#' operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nOM`, `nMP`, and `nPI`)
#' @slot Preset `r preset_param()`
#' @slot Color A character vector length 2 of colors for the maximum and minimum
#' values in the chart.
#' @slot MinValue Numeric vector length `nPI` with the minimum possible value for the
#' respective PIs. Defaults to 0.
#' @slot MaxValue Numeric vector length `nPI` with the maximum possible value (i.e., best performance)
#' for the respective PIs. Defaults to maximum PI value in `Value` (averaged across OMs in some cases).
#'
#' @seealso [Quilt-methods()], [Code()], [Label()], [Description()],
#' [Value()], [Preset()]
#'
#' @example inst/examples/Quilt.R
#' @docType class
#' @export
setClass("Quilt",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Value='array',
                 Preset='list',
                 Color='character_list',
                 MinValue='numeric',
                 MaxValue='numeric'

         )
)


setMethod("initialize", "Quilt", function(.Object,
                                          Code='',
                                          Label='',
                                          Description='',
                                          Value=array(),
                                          Preset=list(),
                                          Color='',
                                          MinValue=0,
                                          MaxValue=numeric()
                                          ) {
  .Object@Code <- Code
  .Object@Label <- Label
  .Object@Description <- Description
  .Object@Value <- Value
  .Object@Preset <- Preset
  .Object@Color <- Color
  .Object@MinValue <- MinValue
  .Object@MaxValue <- MaxValue
  .Object
})


validQuilt <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Quilt', validQuilt)



# Methods ----

## Quilt -----

#' @describeIn Quilt-methods Create an empty `Quilt` object
setMethod("Quilt", 'missing', function() new('Quilt'))

#' @describeIn Quilt-methods Create a populated `Quilt` object
setMethod("Quilt", 'character_list',
          function(Code, Label, Description, Value, Preset, Color, MinValue, MaxValue)
            new('Quilt',Code, Label, Description, Value, Preset, Color, MinValue, MaxValue))


## Code ----


#' @describeIn Code Return `Code` from a [Quilt-class()] object
setMethod("Code", 'Quilt', function(object, lang='en') {
  get_language(object@Code, lang)
})


#' @describeIn Code Assign `Code` to a [Quilt-class()] object
setMethod("Code<-", 'Quilt', function(object, value) {
  object@Code <- value
  methods::validObject(object)
  object
})

## Label ----

#' @describeIn Code Return `Label` from a [Quilt-class()] object
setMethod("Label", 'Quilt', function(object, lang='en') {
  get_language(object@Label, lang)
})


#' @describeIn Code Assign `Label` to a [Quilt-class()] object
setMethod("Label<-", 'Quilt', function(object, value) {
  object@Label <- value
  methods::validObject(object)
  object
})

## Description ----

#' @describeIn Code Return `Description` from a [Quilt-class()] object
setMethod("Description", 'Quilt', function(object, lang='en') {
  get_language(object@Description, lang)
})


#' @describeIn Code Assign `Description` to a [Quilt-class()] object
setMethod("Description<-", 'Quilt', function(object, value) {
  object@Description <- value
  methods::validObject(object)
  object
})

## Value ----
#' @describeIn Value  Return `Value` from a [Quilt-class()] object
setMethod("Value", 'Quilt', function(object) {
  object@Value
})

#' @describeIn Value Assign `Value` to a [Quilt-class()] object
setMethod("Value<-", "Quilt", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

## Preset ----

#' @describeIn Preset Return `Preset` from a [Quilt-class()] object
setMethod("Preset", 'Quilt', function(object) {
  object@Preset
})

#' @describeIn Preset Assign `Preset` to a [Quilt-class()] object
setMethod("Preset<-", "Quilt", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


## Color ----
#' @describeIn Color Return `Color` from a [Quilt-class()] object
setMethod("Color", 'Quilt', function(object) {
  object@Color
})

#' @describeIn Color Preset Assign `Color` to a [Quilt-class()] object
setMethod("Color<-", "Quilt", function(object, value) {
  object@Color <- value
  methods::validObject(object)
  object
})


## MinValue ----

#' Assign and access `MinValue` and `MaxValue` for a `Quilt` object
#'
#' @param Quilt A [Quilt-class()] object
#' @param value A numeric vector with minimum or maximum values for the Performance Indicators.
#' @name MinValue
#' @seealso [Quilt()]
#'
NULL

#' @describeIn MinValue Return `MinValue` from a [Quilt-class()] object
#' @export
MinValue <- function(Quilt) {
  Quilt@MinValue
}

#' @describeIn MinValue Assign `MinValue` to a [Quilt-class()] object
#' @export
`MinValue<-` <- function(Quilt, value) {
  Quilt@MinValue <- value
  methods::validObject(Quilt)
  Quilt
}

## MaxValue ----

#' @describeIn MinValue Return `MaxValue` from a [Quilt-class()] object
#' @export
MaxValue <- function(Quilt) {
  Quilt@MaxValue
}

#' @describeIn MinValue Assign `MaxValue` to a [Quilt-class()] object
#' @export
`MaxValue<-` <- function(Quilt, value) {
  Quilt@MaxValue <- value
  methods::validObject(Quilt)
  Quilt
}



## plot ----

#' @export
setMethod("plot", "Quilt", function(x, ...) {
  plotQuilt(x, ...)
})


## Metadata ----

#' @describeIn Metadata Return Metadata for [Quilt-class()] objects
#' @export
setMethod('Metadata', 'Quilt', function(object, lang='en') {
  data.frame(Code=object@Code,
             Label=get_language(object@Label, lang),
             Description=get_language(object@Description, lang))

})

#' @describeIn Metadata Assign Metadata for [Quilt-class()] objects
setMethod("Metadata<-", "Quilt", function(object, value) {

  names <- c('Code', 'Label', 'Description')
  object <- check_assign_dataframe(object, names, value)
  methods::validObject(object)
  object
})


# Check ----
#' @describeIn Check Check [Quilt-class()] objects for errors
setMethod('Check', 'Quilt', function(object, skip_warnings) {

  ll <- CheckList()
  ll@object <- class(object)

  ll@empty <- is_empty(object)
  if (ll@empty) return(ll)
  ll@empty <- FALSE

  # ll@errors <- append(ll@errors, check_metadata(object))
  #
  # nOM <- max(length(object@Code),
  #             length(object@Label),
  #             length(object@Description)
  # )
  #
  #
  # if (nMPs>0) {
  #   # check numbers and names
  #   ll@errors <- append(ll@errors, check_Preset(object@Preset, nMPs))
  #   Preset <- object@Preset
  # }
  #
  # if (length(ll@errors)<1)
  #   ll@complete <- TRUE

  ll
})

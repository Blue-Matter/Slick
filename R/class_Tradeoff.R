#' S4 class `Tradeoff`
#'
#' Objects of class `Tradeoff` are used to store information for the Tradeoff plot.
#' Like all S4 objects in `Slick`, slots in this object can be accessed and
#' assigned using functions corresponding to slot name. See [Tradeoff()] and the
#' the `See Also` section below.
#'
#' @details
#' Objects of class `Tradeoff` are created with `Tradeoff()`
#'
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
#' @slot Value A 3 dimensional numeric array with the stochastic performance indicator values for
#' operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nOM`, `nMP`, and `nPI`)
#' @slot Preset `r preset_param()`
#'
#' @seealso [Tradeoff()], [Code()], [Label()], [Description()],
#'  [Value()], [Preset()]
#'
#' @example inst/examples/Tradeoff.R
#' @docType class
#' @export
setClass("Tradeoff",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Value='array',
                 Preset='list'
         )
)

## ---- Initialize ----

setMethod("initialize", "Tradeoff", function(.Object,
                                            Code='',
                                            Label='',
                                            Description='',
                                            Value=array(),
                                            Preset=list()) {
  .Object@Code <- Code
  .Object@Label <- Label
  .Object@Description <- Description
  .Object@Value <- Value
  .Object@Preset <- Preset
  methods::validObject(.Object)
  .Object
})

validTradeoff <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('Tradeoff', validTradeoff)


# ---- Methods ----

#' @describeIn Tradeoff-methods Create an empty `Tradeoff` object
setMethod("Tradeoff", 'missing', function() new('Tradeoff'))

#' @describeIn Tradeoff-methods Create a populated `Tradeoff` object
setMethod("Tradeoff", c('character'),
          function(Code, Label, Description, Value, Preset)
            new('Tradeoff', Code, Label, Description, Value, Preset))

#' @describeIn Tradeoff-methods Create a populated `Tradeoff` object
setMethod("Tradeoff", c('list'),
          function(Code, Label, Description, Value, Preset)
            new('Tradeoff', Code, Label, Description, Value, Preset))



# ---- Check ----
#' @describeIn Check Check [Tradeoff-class()] objects for errors
setMethod('Check', 'Tradeoff', function(object) {

  ll <- CheckList()
  ll@object <- class(object)

  ll@empty <- is_empty(object)
  if (ll@empty) return(ll)
  ll@empty <- FALSE

  # check metadata errors
  ll@errors <- append(ll@errors, check_metadata(object))

  # check metadata complete
  if (any(nchar(object@Code)<1))
    ll@warnings <- append(ll@warnings, '`Code` is required')

  if (any(nchar(object@Label)<1))
    ll@warnings <- append(ll@warnings, '`Label` is required')

  if (is_empty_value(object@Value))
    ll@warnings <- append(ll@warnings, '`Value` is required')

  if (!is_empty_value(object@Value) & all(is.na(object@Value)))
    ll@warnings <- append(ll@warnings, '`Value` is all NAs')

  nPI <- NA
  if (is.list(object@Code)) {
    if (length(object@Code[[1]])>0 & all(nchar(object@Code[[1]])>0))
      nPI <- length(object@Code[[1]])
  } else {
    if (length(object@Code)>0 & all(nchar(object@Code)>0))
      nPI <- length(object@Code)
  }

  req_dimensions <- c(NA, NA, nPI)
  ll@warnings <- append(ll@warnings, check_Value(object@Value, req_dimensions))

  if (length(ll@errors)<1 & length(ll@warnings)<1)
    ll@complete <- TRUE

  ll


  ll
})

## Code ----

#' @describeIn Code Return `Code` from a [Tradeoff-class()] object
setMethod("Code", 'Tradeoff', function(object, lang='en') {
  get_language(object@Code, lang)
})


#' @describeIn Code Assign `Code` to a [Tradeoff-class()] object
setMethod("Code<-", 'Tradeoff', function(object, value) {
  object@Code <- value
  methods::validObject(object)
  object
})

## Description ----

#' @describeIn Code Return `Description` from a [Tradeoff-class()] object
setMethod("Description", 'Tradeoff', function(object, lang='en') {
  get_language(object@Description, lang)
})


#' @describeIn Code Assign `Description` to a [Tradeoff-class()] object
setMethod("Description<-", 'Tradeoff', function(object, value) {
  object@Description <- value
  methods::validObject(object)
  object
})

## Value ----
#' @describeIn Value  Return `Value` from a [Tradeoff-class()] object
setMethod("Value", 'Tradeoff', function(object) {
  object@Value
})

#' @describeIn Value Assign `Value` to a [Tradeoff-class()] object
setMethod("Value<-", "Tradeoff", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

## Label ----

#' @describeIn Code Return `Label` from a [Tradeoff-class()] object
setMethod("Label", 'Tradeoff', function(object, lang='en') {
  get_language(object@Label, lang)
})


#' @describeIn Code Assign `Label` to a [Tradeoff-class()] object
setMethod("Label<-", 'Tradeoff', function(object, value) {
  object@Label <- value
  methods::validObject(object)
  object
})


## Preset ----

#' @describeIn Preset Return `Preset` from a [Tradeoff-class()] object
setMethod("Preset", 'Tradeoff', function(object) {
  object@Preset
})

#' @describeIn Preset Assign `Preset` to a [Tradeoff-class()] object
setMethod("Preset<-", "Tradeoff", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})

## Metadata ----

#' @describeIn Metadata Return Metadata for [Tradeoff-class()] objects
#' @export
setMethod('Metadata', 'Tradeoff', function(object, lang='en') {
  data.frame(Code=object@Code,
             Label=get_language(object@Label, lang),
             Description=get_language(object@Description, lang))

})

#' @describeIn Metadata Assign Metadata for [Tradeoff-class()] objects
setMethod("Metadata<-", "Tradeoff", function(object, value) {

  names <- c('Code', 'Label', 'Description')
  object <- check_assign_dataframe(object, names, value)
  methods::validObject(object)
  object
})


## --- Show ----

#' @describeIn show Print a [Tradeoff-class()] object
setMethod("show", "Tradeoff", function(object) {
  dim_names <- c("nOM", "nMP", "nPI")

  chk <- print_show_heading(object)
  if (length(chk@errors)>0)
    print_errors(chk@errors)
  print_metadata(object@Code)
  print_metadata(object@Label, 'Label')
  print_metadata(object@Description, 'Description')

  print_value(object, dim_names)
  print_preset(object@Preset)

})





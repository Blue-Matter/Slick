
# ---- Class ----

#' `Boxplot` S4 Object Class
#'
#' Objects of class `Boxplot` are used to store information for the Boxplot and Violin plots.
#' Like all S4 objects in `Slick`, slots in this object can be accessed and
#' assigned using functions corresponding to slot name. See [Boxplot()] and the
#' the `See Also` section below.
#'
#' @details
#' Objects of class `Boxplot` are created with `Boxplot()`
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
#' @slot Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, and `nPI`).
#' @slot Preset `r preset_param()`
#' @slot Defaults A list object with default selections for the Boxplot. See [Boxplot()]
#'
#' @seealso [Boxplot()], [Code()], [Label()], [Description()],
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
                 Preset='list',
                 Defaults='list'
         )
)

## ---- Initialize ----
setMethod("initialize", "Boxplot", function(.Object,
                                            Code='',
                                            Label='',
                                            Description='',
                                            Value=array(),
                                            Preset=list(),
                                            Defaults=list('overall', 'boxplot')) {
  .Object@Code <- Code
  .Object@Label <- Label
  .Object@Description <- Description
  .Object@Value <- Value
  .Object@Preset <- Preset
  .Object@Defaults <- Defaults
  methods::validObject(.Object)
  .Object
})

## ---- Validate ----
validBoxplot <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('Boxplot', validBoxplot)


# ---- Methods ----

## Boxplot ----

#' @describeIn Boxplot-methods Create an empty `Boxplot` object
setMethod("Boxplot", 'missing', function() new('Boxplot'))

#' @describeIn Boxplot-methods Create a populated `Boxplot` object
setMethod("Boxplot", 'character_list',
          function(Code, Label, Description, Value, Preset, Defaults)
            new('Boxplot', Code, Label, Description, Value, Preset, Defaults))



## Check ----

#' @describeIn Check Check [Boxplot-class()] objects for errors
#' @param req_dims Required dimensions for `Value` slot. Used internally
setMethod('Check', 'Boxplot', function(object, req_dims=c(NA, NA, NA, NA)) {
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

  req_dims[4] <- nPI
  ll@errors <- append(ll@errors, check_Value(object@Value, req_dims))

  if (length(ll@errors)<1 & length(ll@warnings)<1)
    ll@complete <- TRUE


  ll
})


## Code ----

#' @describeIn Code Return `Code` from a [Boxplot-class()] object
setMethod("Code", 'Boxplot', function(object, lang='en') {
  get_language(object@Code, lang)
})


#' @describeIn Code Assign `Code` to a [Boxplot-class()] object
setMethod("Code<-", 'Boxplot', function(object, value) {
  object@Code <- value
  methods::validObject(object)
  object
})

## Description ----

#' @describeIn Code Return `Description` from a [Boxplot-class()] object
setMethod("Description", 'Boxplot', function(object, lang='en') {
  get_language(object@Description, lang)
})


## Label ----

#' @describeIn Code Return `Label` from a [Boxplot-class()] object
setMethod("Label", 'Boxplot', function(object, lang='en') {
  get_language(object@Label, lang)
})


#' @describeIn Code Assign `Label` to a [Boxplot-class()] object
setMethod("Label<-", 'Boxplot', function(object, value) {
  object@Label <- value
  methods::validObject(object)
  object
})



#' @describeIn Code Assign `Description` to a [Boxplot-class()] object
setMethod("Description<-", 'Boxplot', function(object, value) {
  object@Description <- value
  methods::validObject(object)
  object
})

## Metadata ----

#' @describeIn Metadata Return Metadata for [Boxplot-class()] objects
#' @export
setMethod('Metadata', 'Boxplot', function(object, lang='en') {
  data.frame(Code=object@Code,
             Label=get_language(object@Label, lang),
             Description=get_language(object@Description, lang))

})


#' @describeIn Metadata Assign Metadata for [Boxplot-class()] objects
setMethod("Metadata<-", "Boxplot", function(object, value) {

  names <- c('Code', 'Label', 'Description')
  object <- check_assign_dataframe(object, names, value)
  methods::validObject(object)
  object
})


## Preset ----

#' @describeIn Preset Return `Preset` from a [Boxplot-class()] object
setMethod("Preset", 'Boxplot', function(object) {
  object@Preset
})

#' @describeIn Preset Assign `Preset` slot from a [Boxplot-class()] object
setMethod("Preset<-", "Boxplot", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


## Defaults ----

#' @describeIn Defaults Defaults `Code` from a [Boxplot-class()] object
setMethod("Defaults", 'Boxplot', function(object) {
  object@Defaults
})


#' @describeIn Defaults Assign `Defaults` to a [Boxplot-class()] object
setMethod("Defaults<-", 'Boxplot', function(object, value) {
  object@Defaults <- value
  methods::validObject(object)
  object
})



## Show ----

#' @describeIn show Print a [Boxplot-class()] object
setMethod("show", "Boxplot", function(object) {
  dim_names <- c("nsim", "nOM", "nMP", "nPI")

  chk <- print_show_heading(object)
  if (length(chk@errors)>0)
    print_errors(chk@errors)
  print_metadata(object@Code)
  print_metadata(object@Label, 'Label')
  print_metadata(object@Description, 'Description')
  print_value(object, dim_names)
  print_preset(object@Preset)
})


## Value ----
#' @describeIn Value  Return `Value` from a [Boxplot-class()] object
setMethod("Value", 'Boxplot', function(object) {
  object@Value
})

#' @describeIn Value Assign `Value` to a [Boxplot-class()] object
setMethod("Value<-", "Boxplot", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})





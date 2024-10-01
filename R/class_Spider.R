#' S4 class `Spider`
#'
#' Objects of class `Spider` are used to store information for the Spider plots.
#' Like all S4 objects in `Slick`, slots in this object can be accessed and
#' assigned using functions corresponding to slot name. See [Spider-methods()] and the
#' the `See Also` section below.
#'
#' @details
#' Objects of class `Spider` are created with `Spider()`
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
#' Dimensions: c(`nOM`, `nMP`, and `nPI`).
#' All PI values must range between 0 and 1 or 0 and 100. If all values are <= 1, they will be
#' multiplied by 100 in the plot.
#' Dimensions: c(`nOM`, `nMP`, and `nPI`)
#' @slot Preset `r preset_param()`
#'
#' @seealso [Spider-methods()], [Code()], [Label()], [Description()],
#' [Value()], [Preset()]
#'
#' @example inst/examples/Spider
#' @docType class
#' @export
setClass("Spider",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Value='array',
                 Preset='list'
         )
)


setMethod("initialize", "Spider", function(.Object,
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


validSpider <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('Spider', validSpider)

# ---- Methods ----

#' @describeIn Spider-methods Create an empty `Spider` object
setMethod("Spider", 'missing', function() new('Spider'))

#' @describeIn Spider-methods Create a populated `Spider` object
setMethod("Spider", c('character'),
          function(Code, Label, Description, Value, Preset)
            new('Spider', Code, Label, Description, Value, Preset))

#' @describeIn Spider-methods Create a populated `Spider` object
setMethod("Spider", c('list'),
          function(Code, Label, Description, Value, Preset)
            new('Spider', Code, Label, Description, Value, Preset))



## ---- Check ----

#' @describeIn Check Check [Spider-class()] objects for errors
setMethod('Check', 'Spider', function(object) {

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

#' @describeIn Code Return `Code` from a [Spider-class()] object
setMethod("Code", 'Spider', function(object, lang='en') {
  get_language(object@Code, lang)
})


#' @describeIn Code Assign `Code` to a [Spider-class()] object
setMethod("Code<-", 'Spider', function(object, value) {
  object@Code <- value
  methods::validObject(object)
  object
})

## Description ----

#' @describeIn Code Return `Description` from a [Spider-class()] object
setMethod("Description", 'Spider', function(object, lang='en') {
  get_language(object@Description, lang)
})


#' @describeIn Code Assign `Description` to a [Spider-class()] object
setMethod("Description<-", 'Spider', function(object, value) {
  object@Description <- value
  methods::validObject(object)
  object
})


## Label ----

#' @describeIn Code Return `Label` from a [Spider-class()] object
setMethod("Label", 'Spider', function(object, lang='en') {
  get_language(object@Label, lang)
})


#' @describeIn Code Assign `Label` to a [Spider-class()] object
setMethod("Label<-", 'Spider', function(object, value) {
  object@Label <- value
  methods::validObject(object)
  object
})


## Metadata ----

#' @describeIn Metadata Return Metadata for [Spider-class()] objects
#' @export
setMethod('Metadata', 'Spider', function(object, lang='en') {
  data.frame(Code=object@Code,
             Label=get_language(object@Label, lang),
             Description=get_language(object@Description, lang))

})

#' @describeIn Metadata Assign Metadata for [Spider-class()] objects
setMethod("Metadata<-", "Spider", function(object, value) {

  names <- c('Code', 'Label', 'Description')
  object <- check_assign_dataframe(object, names, value)
  methods::validObject(object)
  object
})



## Show ----

#' @describeIn show Print a [Spider-class()] object
setMethod("show", "Spider", function(object) {
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




## Preset ----

#' @describeIn Preset Return `Preset` from a [Spider-class()] object
setMethod("Preset", 'Spider', function(object) {
  object@Preset
})

#' @describeIn Preset Assign `Preset` slot from a [Spider-class()] object
setMethod("Preset<-", "Spider", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


## Value ----
#' @describeIn Value  Return `Value` from a [Spider-class()] object
setMethod("Value", 'Spider', function(object) {
  object@Value
})

#' @describeIn Value Assign `Value` to a [Spider-class()] object
setMethod("Value<-", "Spider", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})






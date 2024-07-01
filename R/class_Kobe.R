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
#' By default, the first PI will be on the x-axis (usually B/BMSY or something similar) and the second
#' on the y-axis (e.g., F/FMSY). The Slick [App()] provides dropdowns for selecting other PIs.
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
#' @slot Target Numeric vector length `nPI` with the target value for the PIs. Defines the color quadrants on the Kobe plot.
#'  Defaults to 1.
#' @slot Limit Numeric vector length `nPI` with the limit value for the two PIs. Shows as red line on Kobe plot. NULL to ignore.
#' @slot Defaults A list object with default selections for the Kobe See [Kobe()]
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
                 Limit='numericOrNULL',
                 Defaults='list'
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
                                         Limit=NULL,
                                         Defaults=list('overall')) {
  .Object@Code <- Code
  .Object@Label <- Label
  .Object@Description <- Description
  .Object@Time <- Time
  .Object@TimeLab <- TimeLab
  .Object@Value <- Value
  .Object@Preset <- Preset
  .Object@Target <- Target
  .Object@Limit <- Limit
  .Object@Defaults <- Defaults
  methods::validObject(.Object)
  .Object
})

## ---- Validate ----
validKobe <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('Kobe', validKobe)


# ---- Methods ----
## Kobe ----
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



## Check ----
#' @describeIn Check Check [Kobe-class()] objects for errors
setMethod('Check', 'Kobe', function(object) {

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

  # Time
  time <- Time(object)
  if (length(time)<1)
    ll@warnings <- append(ll@warnings, '`Time` is required')

  nTS <- NA
  if (length(Time(object))>0) {
    nTS <- length(Time(object))
  }
  req_dimensions <- c(NA, NA, NA, nPI, nTS)
  ll@warnings <- append(ll@warnings, check_Value(object@Value, req_dimensions))

  # Target
  if (!is.null(object@Target) & all(nchar(Code(object))>0)) {
    targ <- object@Target
    if (length(targ)>1 & length(targ)!=length(Code(object)))
      ll@errors <- append(ll@errors, list(Target=paste0('`Target` must be length 1 or length `Code` (',
                                                        length(Code(object)), ')'))
      )
  }

  # Limit
  if (!is.null(object@Limit) & all(nchar(Code(object))>0)) {
    targ <- object@Limit
    if (length(targ)>1 & length(targ)!=length(Code(object)))
      ll@errors <- append(ll@errors, list(Limit=paste0('`Limit` must be length 1 or length `Code` (',
                                                        length(Code(object)), ')'))
      )
  }

  if (length(ll@errors)<1 & length(ll@warnings)<1)
    ll@complete <- TRUE


  ll
})

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





##  Show ----

#' @param object An object of class [Kobe-class()]
#' @export
setMethod("show", "Kobe", function(object) {
  dim_names <- c("nsim", "nOM", "nMP", "nPI", 'nTS')

  chk <- print_show_heading(object)
  if (length(chk@errors)>0)
    print_errors(chk@errors)
  print_metadata(object@Code)
  print_metadata(object@Label, 'Label')
  print_metadata(object@Description, 'Description')

  cli::cli_h2('{.code Time}')
  tt <- Time(object)
  if (length(tt)>0) {
    if (length(tt)>2) {
      t1 <- paste(tt[1:2], collapse=', ')
      t2 <- paste(tt[(length(tt)-1):(length(tt))], collapse=', ')
    } else {
      t1 <- tt[1]
      t2 <- tt[2]
    }
    tval <- paste0(t1, ', ... , ', t2)
    cli::cli_inform(tval)
  }
  cli::cli_h2('{.code TimeLab}')
  cli::cli_inform(TimeLab(object))

  print_value(object, dim_names)
  print_preset(object@Preset)

  cli::cli_h2('{.code Target}')

  if (!is.null(object@Target) & all(nchar(Code(object))>0)) {
    targ <- object@Target
    if (length(targ)==1)
      targ <- rep(targ, length(Code(object)))
    names(targ) <- Code(object)
    print(targ)
  }
  cli::cli_h2('{.code Limit}')
  if (!is.null(object@Limit) & all(nchar(Code(object))>0)) {
    targ <- object@Limit
    if (length(targ)==1)
      targ <- rep(targ, length(Code(object)))
    names(targ) <- Code(object)
    print(targ)
  }

})



## Defaults ----

#' @describeIn Defaults Defaults `Code` from a [Kobe-class()] object
setMethod("Defaults", 'Kobe', function(object) {
  object@Defaults
})


#' @describeIn Defaults Assign `Defaults` to a [Kobe-class()] object
setMethod("Defaults<-", 'Kobe', function(object, value) {
  object@Defaults <- value
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





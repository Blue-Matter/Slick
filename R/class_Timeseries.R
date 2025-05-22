
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
#' @slot RefPoints List for setting custom Reference Points. Overrides `Target` and `Limit`.
#'  See `Details` section in [Timeseries()].
#' @seealso [Timeseries()], [Code()], [Label()], [Description()],
#'  [Value()], [Preset()]
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
                 Limit='numericOrNULL',
                 RefPoints='list'
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
                                            Limit=NULL,
                                            RefPoints=list()) {
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
  .Object@RefPoints <- RefPoints
  methods::validObject(.Object)
  .Object
})


validTimeSeries <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('Timeseries', validTimeSeries)

# ---- Methods ----

#' @describeIn Timeseries-methods Create an empty `Timeseries` object
setMethod("Timeseries", 'missing', function() new('Timeseries'))

#' @describeIn Timeseries-methods Create a populated `Timeseries` object
setMethod("Timeseries", c('character'),
          function(Code, Label, Description, Time, TimeNow, TimeLab,
                   Value, Preset, Target, Limit, RefPoints)
            new('Timeseries', Code, Label, Description, Time, TimeNow, TimeLab,
                Value, Preset, Target, Limit, RefPoints))

#' @describeIn Timeseries-methods Create a populated `Timeseries` object
setMethod("Timeseries", c('list'),
          function(Code, Label, Description, Time, TimeNow, TimeLab,
                   Value, Preset, Target, Limit, RefPoints)
            new('Timeseries', Code, Label, Description, Time, TimeNow, TimeLab,
                Value, Preset, Target, Limit, RefPoints))


## Check ----
#' @describeIn Check Check [Timeseries-class()] objects for errors
setMethod('Check', 'Timeseries', function(object) {

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

  nTS <- NA
  if (length(Time(object))>0) {
    nTS <- length(Time(object))
  }
  req_dimensions <- c(NA, NA, NA, nPI, nTS)
  ll@warnings <- append(ll@warnings, check_Value(object@Value, req_dimensions))


  if (!is.na(nTS) & length(TimeNow(object)>0)) {
    if (!TimeNow(object) %in% Time(object))
      ll@errors <- append(ll@errors, list(TimeNow='`TimeNow` must be a numeric value within `Time`'))
  }


  # Time
  time <- Time(object)
  if (length(time)<1)
    ll@warnings <- append(ll@warnings, '`Time` is required')


  # TimeNow
  timenow <- TimeNow(object)
  if (length(timenow)<1)
    ll@warnings <- append(ll@warnings, '`TimeNow` is required')

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

  ## RefPoints
  if (length(object@RefPoints) & all(nchar(Code(object))>0)) {
    if (length(object@RefPoints) > length(Code(object))) {
      ll@errors <- append(ll@errors, list(RefPoints=paste0('Length of `RefPoints` list must be <= length `Code` (',
                                                       length(Code(object)), ')'))
      )
      # for (i in seq_along(object@RefPoints)) {
      #   nms <- names(object@RefPoints[[i]])
      #   validnames <- c('Name', 'Value', 'Color')
      #   if (length(nms[!nms %in%validnames])>0) {
      #
      #   }
      # }

    }


  }

  if (length(ll@errors)<1 & length(ll@warnings)<1)
    ll@complete <- TRUE


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
#' @return A `data.frame`
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

## RefPoints ----

#' @describeIn RefPoints Return `RefPoints` from a [Timeseries-class()] object
setMethod("RefPoints", 'Timeseries', function(object) {
  object@RefPoints
})

#' @describeIn RefPoints Assign `RefPoints` to a [Timeseries-class()] object
setMethod("RefPoints<-", "Timeseries", function(object, value) {
  if (is.null(value)) return(object)
  object@RefPoints <- value
  methods::validObject(object)
  object
})

##  Show ----

#' @describeIn show Print a [Timeseries-class()] object
setMethod("show", "Timeseries", function(object) {
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
  cli::cli_h2('{.code TimeNow}')
  cli::cli_inform(TimeNow(object))

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
#' @return A numeric value
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
setMethod("TimeLab", "Timeseries", function(object, lang='en') {
  get_language(object@TimeLab, lang)
})


#' @describeIn TimeLab Assign `TimeLab` to a [Timeseries-class()] object
setMethod("TimeLab<-", "Timeseries", function(object, value) {
  object@TimeLab <- value
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

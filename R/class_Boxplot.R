
# ---- Class ----

#' S4 class `Boxplot`
#'
#' Objects of class `Boxplot` are used to store information for the Boxplot and Violin plots.
#' Like all S4 objects in `Slick`, slots in this object can be accessed and
#' assigned using functions corresponding to slot name. See [Boxplot()] and the
#' the `See Also` section below.
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
  errors <- list()
  if (length(errors)>0)
    return(errors)
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
setMethod('Check', 'Boxplot', function(object, skip_warnings) {

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



## Show ----

#' @describeIn Boxplot Show objects of class `Boxplot`
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

  # if (!(all(apply(metadata,1, nchar)[,1]==0)) && (length(dd) == length(dims)) & !all(is.na(object@Value))) {
  #   if (nrow(metadata) != dd[4])
  #     cat('\nWARNING:\n The PI dimension in `Value` (dimension 4) does not match the number of performance indicators described in `Code`, `Label`, and `Description`\n')
  # }


  preset <- object@Preset
  cat('\nPresets: \n')
  if (length(preset)<1) {
    cat('None')
  } else {
    print(object@Preset)
  }
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





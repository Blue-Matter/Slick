# ---- Class ----

#' `MPs` S4 class and functions
#'
#' An object of class `MPs` contains information about the management procedures (MPs)
#' in a [Slick-class()] object. Like all S4 objects in `Slick`, slots in this
#' object can be accessed and assigned using functions corresponding to slot name.
#' See [MPs()] and the the `See Also` section below.
#'
#' @slot Code `r code_MP_param()` *Required*
#' @slot Label `r label_MP_param()` *Required*
#' @slot Description `r description_MP_param()`
#' @slot Color A character vector of colors for the MPs. Defaults will be used if not populated
#' @slot Preset `r preset_param()`
#'
#' @details
#'
#' Objects of class `MPs` are created with `MPs()`
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of management procedures (`nMPs`) in the plot objects `Boxplot`,
#' `Kobe`, `Quilt`, `Spider`, `Timeseries`, and `Tradeoff`.
#'
#'
#' @example inst/examples/MPs.R
#' @seealso [Code()], [Label()], [Description()], [Preset()]
#' @export
setClass("MPs",
         slots=c(Code='character_list',
                 Label='character_list',
                 Description='character_list',
                 Color='character',
                 Preset='list'
         )
)


setMethod("initialize", "MPs", function(.Object,
                                        Code='',
                                        Label='',
                                        Description='',
                                        Color='',
                                        Preset=list()) {
  .Object@Code <- Code
  .Object@Label <- Label
  .Object@Description <- Description
  if (all(nchar(Color)==0) & length(Code)>1) {
    nMP <- length(Code)
    Color <- default_mp_colors(nMP)
  }
  .Object@Color <- Color
  .Object@Preset <- Preset
  methods::validObject(.Object)
  .Object
})

## Validate ----

validMPs <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('MPs', validMPs)



# Methods ----

#' @describeIn MPs-methods Create an empty `MPs` object
setMethod("MPs", 'missing', function() new('MPs'))

#' @describeIn MPs-methods Create a populated `MPs` object
setMethod("MPs", c('character_list'),
          function(Code, Label, Description, Color, Preset)
            new('MPs', Code, Label, Description, Color, Preset))



## Check ----
#' @describeIn Check Check [MPs-class()] objects for errors
setMethod('Check', 'MPs', function(object) {

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


  nMPs <- max(length(object@Code),
              length(object@Label),
              length(object@Description)
              )

  if (any(nchar(object@Color)>0)) {
    if (length(object@Color) != nMPs)
      ll@errors <- append(ll@errors, list(Color= paste0('`Color` must be length `nMPs` (', nMPs, ')')))
  }


  if (nMPs>0) {
    # check numbers and names
    #ll@errors <- append(ll@errors, check_Preset(object@Preset, nMPs))
    # Preset <- object@Preset
  }

  if (length(ll@errors)<1 & length(ll@warnings)<1)
    ll@complete <- TRUE

  ll
})


## Code ----


#' @describeIn Code Return `Code` from a [MPs-class()] object
setMethod("Code", 'MPs', function(object, lang='en') {
  get_language(object@Code, lang)
})


#' @describeIn Code Assign `Code` to a [MPs-class()] object
setMethod("Code<-", 'MPs', function(object, value) {
  object@Code <- value

  # add default colors
  if (any(nchar(object@Code))>0) {
    if (any(nchar(object@Color))<1) {
      object@Color <- default_mp_colors(length(object@Code))
    }
  }

  methods::validObject(object)
  object
})

## Label ----

#' @describeIn Code Return `Label` from a [MPs-class()] object
setMethod("Label", 'MPs', function(object, lang='en') {
  get_language(object@Label, lang)
})


#' @describeIn Code Assign `Label` to a [MPs-class()] object
setMethod("Label<-", 'MPs', function(object, value) {
  object@Label <- value
  methods::validObject(object)
  object
})

## Description ----

#' @describeIn Code Return `Description` from a [MPs-class()] object
setMethod("Description", 'MPs', function(object, lang='en') {
  get_language(object@Description, lang)
})


#' @describeIn Code Assign `Description` to a [MPs-class()] object
setMethod("Description<-", 'MPs', function(object, value) {
  object@Description <- value
  methods::validObject(object)
  object
})



## Preset ----

#' @describeIn Preset Return `Preset` from a [MPs-class()] object
setMethod("Preset", 'MPs', function(object) {
  object@Preset
})

#' @describeIn Preset Assign `Preset` to a [MPs-class()] object
setMethod("Preset<-", "MPs", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


## Color ----
#' @describeIn Color Return `Color` from a [MPs-class()] object
setMethod("Color", 'MPs', function(object) {
  object@Color
})

#' @describeIn Color Preset Assign `Color` to a [MPs-class()] object
setMethod("Color<-", "MPs", function(object, value) {
  object@Color <- value
  methods::validObject(object)
  object
})



## Show ----

#' @export
setMethod("show", "MPs", function(object) {
  chk <- print_show_heading(object)
  if (length(chk@errors)>0)
    print_errors(chk@errors)
  print_metadata(object@Code)
  print_metadata(object@Label, 'Label')
  print_metadata(object@Description, 'Description')
  print_metadata(object@Color, 'Color')
  print_preset(object@Preset)
})






## Metadata ----

#' @describeIn Metadata Return Metadata for [MPs-class()] objects
#' @export
setMethod('Metadata', 'MPs', function(object, lang='en') {
  data.frame(Code=object@Code,
             Label=get_language(object@Label, lang),
             Description=get_language(object@Description, lang),
             Color=object@Color)

})


#' @describeIn Metadata Assign Metadata for [MPs-class()] objects
setMethod("Metadata<-", "MPs", function(object, value) {

  names <- c('Code', 'Label', 'Description')
  object <- check_assign_dataframe(object, names, value)
  if (!is.null(value$Color)) {
    object@Color <- value$Color
  }
  # add default colors
  if (any(nchar(object@Code))>0) {
    if (any(nchar(object@Color))<1) {
      object@Color <- default_mp_colors(length(object@Code))
    }
  }

  methods::validObject(object)
  object
})













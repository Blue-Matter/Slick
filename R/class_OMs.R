# ---- Class ----

#' `OMs` S4 class and functions
#'
#' An object of class `OMs` contains information about the operating models (MPs)
#' in the [Slick()] object. Like all S4 objects in `Slick`, slots in this
#' object can be accessed and assigned using functions corresponding to slot name.
#' See [OMs()] and the the `See Also` section below.
#'
#' @slot Factors A `data.frame` with column headings `Factor`, `Level`, and `Description`. See `Details`
#' @slot Design A `data.frame` with `nFactor` columns (i.e., `length(unique(Factors$Factor))`), and `nOM`
#' rows. See `Details`
#' @slot Preset `r preset_param()`
#'
#' @details
#'
#' ## Multi-Language Support
#' Use a named list to use multi-languages in `Factors`
#'
#' ## Factors
#' `Factors` can be accessed and assigned using `Factors(myslick)` and
#' `Factors(myslick) <- data.frame()` respectively.
#'
#' The `Factor` column should be character strings with the name of each factor,
#' while the `Level` column is a `numeric` or `character` value with the level for the
#' corresponding factor.
#'
#' The `Description` column is a description for each row, i.e., a unique factor and level.
#' See `Examples`.
#'
#' ## Design
#' The `Design` matrix is `nOM` rows and `nFactor` columns. The values in each column should
#' either be `numeric` values indicating the levels for the corresponding factor,
#' or the actual level values (i.e., `Factors$Level`) that correspond to each OM. See `Examples`.
#'
#' @seealso [OMs-methods()], [Factors()], [Design()], [Preset()]
#'
#' @example inst/examples/OMs.R
#' @docType class
#' @export
setClass("OMs",
                slots=c(Factors='dataframe_list',
                        Design='data.frame',
                        Preset='list'
                )
)


## ---- Initialize ----

setMethod("initialize", "OMs", function(.Object,
                                        Factors=data.frame(),
                                        Design=data.frame(),
                                        Preset=list()) {
  .Object@Factors <- Factors
  .Object@Design <- Design
  .Object@Preset <- Preset
  methods::validObject(.Object)
  .Object
})

## ---- Validate ----

validOMs <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
}

setValidity('OMs', validOMs)


# ---- Methods ----

#' @describeIn OMs-methods Create an empty `OMs` object
setMethod("OMs", 'missing', function() new('OMs'))

#' @describeIn OMs-methods Create a populated `OMs` object
setMethod("OMs", c('dataframe_list'),
          function(Factors, Design, Preset)
            new('OMs', Factors, Design, Preset))



#' @describeIn Design Return the operating model `Design` matrix from a [OMs-class()] object
setMethod('Design', 'OMs', function(object) object@Design)


#' @describeIn Design Assign the operating model `Design` matrix to a [OMs-class()] object
setMethod('Design<-', 'OMs', function(object, value) {
  object@Design <- value
  methods::validObject(object)
  object
})

#' @describeIn Factors Return the operating model `Factors` table from a [OMs-class()] object
setMethod('Factors', 'OMs', function(object, lang='en')
  get_language(object@Factors,lang)
)


#' @describeIn Factors Assign the operating model `Factors` table  to a [OMs-class()] object
setMethod('Factors<-', 'OMs', function(object, value) {
  object@Factors <- value
  methods::validObject(object)
  object
})


#' @describeIn Preset Return `Preset` from a [OMs-class()] object
setMethod("Preset", 'OMs', function(object) {
  object@Preset
})


#' @describeIn Preset Assign `Preset` to an `OMs-class()` object
#' @export
setMethod("Preset<-", "OMs", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})




## ---- Check ----

#' @describeIn Check Check [OMs-class()] objects for errors
setMethod('Check', 'OMs', function(object) {

  ll <- CheckList()
  ll@object <- class(object)

  ll@empty <- is_empty(object)
  if (ll@empty) return(ll)
  ll@empty <- FALSE

  # check Factors
  ll@errors <- append(ll@errors, check_factors(object))

  if (is.list(object@Factors) & !is.data.frame(object@Factors)) {
    if (length(object@Factors)<1) {
      ll@warnings  <- append(ll@warnings, '`Factors` required')
    } else {
      if (nrow(object@Factors[[1]])<1)
        ll@warnings  <- append(ll@warnings, '`Factors` required')
    }
  } else {
    if (nrow(object@Factors)<1)
      ll@warnings  <- append(ll@warnings, '`Factors` required')
  }

  # check Design
  ll@errors <- append(ll@errors, check_design(object))
  if (nrow(object@Design)<1)
      ll@warnings  <- append(ll@warnings, '`Design` required')


  if (length(ll@errors)<1 & length(ll@warnings)<1)
    ll@complete <- TRUE

  ll
})




## Show ----

#' @describeIn OMs Show objects of class `OMs`
#' @export
setMethod("show", "OMs", function(object) {
  chk <- print_show_heading(object)
  if (length(chk@errors)>0)
    print_errors(chk)
  print_factors(object@Factors)
  print_design(object@Design)
  print_preset(object@Preset)
})


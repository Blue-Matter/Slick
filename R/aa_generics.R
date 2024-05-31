
show <- function(object) methods::show(object)

#' @include class_Boxplot.R
#' @include class_Kobe.R
#' @include class_MPs.R
#' @include class_OMs.R
#' @include class_Quilt.R
#' @include class_Slick.R
#' @include class_Spider.R
#' @include class_Tradeoff.R
#' @include class_Timeseries.R
NULL

get_language <- function(value, lang) {
  if (is.null(lang))
    return(value)

  if (inherits(value, 'data.frame')) {
    return(value)
  }
  if (inherits(value, 'list')) {
    if (is.null(names(value)))
      return(value)
    if (!lang %in% names(value)) {
      warning(lang, ' not found. Using default', call.=FALSE)
      return(value[[1]])
    }
    return(value[[lang]])
  }
  value
}






# ---- MPs ----

#' @describeIn MPs A generic function
#' @export
setGeneric("MPs", function(Metadata, value) standardGeneric("MPs"))

#' @param Metadata An object of class [data.frame()] or [list()] formatted for the
#' `Metadata` slot. See `Details`. Or an object of class [Slick].
#' @param value Optional. Either a [list()] with a specific structure,
#' a character string specifying language (if available), or an object of class [MPs()]. See `Details`
#' @rdname MPs
setMethod("MPs", c("missing", 'missing'), function() newMPs())

<<<<<<< Updated upstream
#' @describeIn MPs Create a new object of class [MPs()]
#' @param x An object of class [data.frame()] or [list()]
=======
#' @rdname MPs
#' @export
setMethod("MPs", c("data.frame", 'missing'), function(Metadata) newMPs(Metadata))

#' @rdname MPs
>>>>>>> Stashed changes
#' @export
setMethod("MPs", c("list", 'missing'), function(Metadata) newMPs(Metadata))

#' @describeIn MPs Create a new object of class [MPs()]
#' @param missingOrNULL missing or NULL
#' @param ... additional arguments (not used)
#' @export
setMethod("MPs","missingOrNULL",function(x, ...) newMPs(...))


<<<<<<< Updated upstream
#' @describeIn MPs Return the [MPs()] object from an object of class [Slick()]
#' @param lang optional.character string length 2 to specify language (if available).  'en', 'es', or 'fr'
=======
#' @rdname MPs
>>>>>>> Stashed changes
#' @export
setMethod("MPs", c("data.frame", 'list'), function(Metadata, value) newMPs(Metadata, value))

#' @rdname MPs
#' @export
setMethod("MPs", c("list", 'list'), function(Metadata, value) newMPs(Metadata, value))


#' @rdname MPs
#' @export
setMethod("MPs", c('Slick', 'character'), function(Metadata, value) {
  get_language(Metadata@MPs, lang=value)
} )


#' @rdname MPs
#' @export
setMethod("MPs", c('Slick', 'missing'), function(Metadata) {
  get_language(Metadata@MPs, lang='en')
} )



#' @describeIn MPs Assign an object of class [MPs()] to an object of class [Slick()]
#' @export
setGeneric("MPs<-", function(Slick, value) standardGeneric("MPs<-"))

#' @rdname MPs
#' @param Slick An object of class [Slick()]
<<<<<<< Updated upstream
#' @param value An object of class [MPs()]
=======
>>>>>>> Stashed changes
#' @export
setMethod("MPs<-", "Slick", function(Slick, value) {
  Slick@MPs <- value
  methods::validObject(Slick)
  Slick
})


#
# ---- OMs ----

#' @export
setGeneric("OMs", function(Metadata, Design, Preset) standardGeneric("OMs"))

#' @param Metadata An object of class [data.frame()] or [list()] formatted for the
#' `Metadata` slot. See `Details`. Or an object of class [Slick].
#' @param Design A `data.frame` with the design matrix for the `OMs` object. See `Details`
#' @param Preset An optional named list. See `Details`
#' @rdname OMs
#' @export
setMethod("OMs", c("missing", 'missing', 'missing'), function() newOMs())

#' @rdname OMs
#' @export
setMethod("OMs", c("data.frame", 'missing', 'missing'), function(Metadata) newOMs(Metadata))

#' @rdname OMs
#' @export
setMethod("OMs", c("list", 'missing', 'missing'), function(Metadata) newOMs(Metadata))

#' @rdname OMs
#' @export
setMethod("OMs", c("data.frame", 'data.frame', 'missing'),
          function(Metadata, Design) newOMs(Metadata, Design))


#' @rdname OMs
#' @export
setMethod("OMs", c("data.frame", 'data.frame', 'list'),
          function(Metadata, Design, Preset) newOMs(Metadata, Design, Preset))


#' @rdname OMs
#' @export
setMethod("OMs", c("Slick"),
          function(Metadata) Metadata@OMs)

#' @rdname OMs
#' @param Slick An object of class [Slick]
#' @param value An object of class [OMs]
#' @export
setGeneric("OMs<-", function(Slick, value) standardGeneric("OMs<-"))

#' @rdname OMs
#' @export
setMethod("OMs<-", c("Slick"),
          function(Slick, value) {
            Slick@MPs <- value
            methods::validObject(Slick)
            Slick
})



# ---- Preset ----
setGeneric("Preset", function(object) standardGeneric("Preset"))

#' Assign or access the `Preset` slot
#' @param object An object of class [Boxplot], [Kobe], [Quilt],
#' [Spider], [Timeseries], [Tradeoff], or [MPs]
#'
#' @export
#' @rdname Preset
setMethod("Preset", 'Boxplot', function(object) {
  object@Preset
})

#' @export
#' @rdname Preset
setMethod("Preset", 'Kobe', function(object) {
  object@Preset
})

#' @export
#' @rdname Preset
setMethod("Preset", 'Quilt', function(object) {
  object@Preset
})

#' @export
#' @rdname Preset
setMethod("Preset", 'Spider', function(object) {
  object@Preset
})

#' @export
#' @rdname Preset
setMethod("Preset", 'Timeseries', function(object) {
  object@Preset
})

#' @export
#' @rdname Preset
setMethod("Preset", 'Tradeoff', function(object) {
  object@Preset
})

#' @export
#' @rdname Preset
setMethod("Preset", 'MPs', function(object) {
  object@Preset
})


setGeneric("Preset<-", function(object, value) standardGeneric("Preset<-"))

#' @param value A `list`, formatted to match the class of `object`. See the documentation for
#' corresponding `object` class for more details.
#' @rdname Preset
#' @export
setMethod("Preset<-", "Boxplot", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


#' @rdname Preset
#' @export
setMethod("Preset<-", "Kobe", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})

#' @rdname Preset
#' @export
setMethod("Preset<-", "Quilt", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})

#' @rdname Preset
#' @export
setMethod("Preset<-", "Spider", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})

#' @rdname Preset
#' @export
setMethod("Preset<-", "Timeseries", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})

#' @rdname Preset
#' @export
setMethod("Preset<-", "Tradeoff", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})


#' @rdname Preset
#' @export
setMethod("Preset<-", "MPs", function(object, value) {
  if (is.null(value)) return(object)
  object@Preset <- value
  methods::validObject(object)
  object
})

# ---- Quilt ----
#' @export
setGeneric("Quilt", function(Metadata, Value, Preset, Colors) standardGeneric("Quilt"))

#' @param Metadata An object of class [data.frame()] or [list()] formatted for the
#' `Metadata` slot. See `Details`. Or an object of class [Slick].
#' @param Value An `array` with the values for the `Quilt` chart. See `Details`.
#' a character string specifying language (if available), or an object of class [MPs()]. See `Details`
#' @param Preset An optional named list. See `Details`.
#' @param Colors A character vector of length 2 specifying the colors for
#' the maximum and minimum values in the Quilt chart
#' @rdname Quilt
#' @export
setMethod("Quilt", c("missing", 'missing', 'missing', 'missing'), function() newQuilt())

#' @rdname Quilt
#' @export
setMethod("Quilt", c("data.frame"),
          function(Metadata) newQuilt(Metadata))

#' @rdname Quilt
#' @export
setMethod("Quilt", c("list"),
          function(Metadata) newQuilt(Metadata))

#' @rdname Quilt
#' @export
setMethod("Quilt", c("data.frame", 'array'),
          function(Metadata, Value) newQuilt(Metadata, Value))

#' @rdname Quilt
#' @export
setMethod("Quilt", c("list", 'array'),
          function(Metadata, Value) newQuilt(Metadata, Value))


#' @rdname Quilt
#' @export
setMethod("Quilt", c("data.frame", 'array', 'list', 'missing'),
          function(Metadata, Value, Preset) newQuilt(Metadata, Value, Preset))

#' @rdname Quilt
#' @export
setMethod("Quilt", c("list", 'array', 'list', 'missing'),
          function(Metadata, Value, Preset) newQuilt(Metadata, Value, Preset))


#' @rdname Quilt
#' @export
setMethod("Quilt", c("data.frame", 'array', 'list', 'character'),
          function(Metadata, Value, Preset, Colors) newQuilt(Metadata, Value, Preset, Colors))

#' @rdname Quilt
#' @export
setMethod("Quilt", c("list", 'array', 'list', 'character'),
          function(Metadata, Value, Preset, Colors) newQuilt(Metadata, Value, Preset, Colors))

#' @rdname Quilt
#' @export
setMethod("Quilt", "Slick", function(Metadata) Metadata@Quilt)

setGeneric("Quilt<-", function(Slick, value) standardGeneric("Quilt<-"))

#' @rdname Quilt
#' @param Slick An object of class [Slick]
#' @param value An object of class [Quilt]
#' @export
setMethod("Quilt<-", "Slick", function(Slick, value) {
  object@Quilt <- value
  methods::validObject(object)
  object
})


# ---- Selected ----

setGeneric("Selected", function(Tradeoff) standardGeneric("Selected"))

#' Assign or access the `Selected` slot in an object of class [Tradeoff]
#'
#' @param Tradeoff An object of class [Tradeoff]
#' @examples
#' tradeoff <- Tradeoff()
#' Selected(tradeoff) <- c(1,2)
#' Selected(tradeoff)
#' @rdname Selected
#' @export
setMethod("Selected", "Tradeoff", function(Tradeoff) {
  Tradeoff@Selected
})

setGeneric("Selected<-", function(Tradeoff, value) standardGeneric("Selected<-"))


#' @rdname Selected
#' @param value A character or numeric vector length 2. See [Tradeoff]
#' @export
setMethod("Selected<-", "Tradeoff", function(Tradeoff, value) {
  Tradeoff@Selected <- value
  methods::validObject(Tradeoff)
  Tradeoff
})


# ---- Spider ----

#' @export
setGeneric("Spider", function(Metadata, Value, Preset) standardGeneric("Spider"))

#' @param Metadata An object of class [data.frame()] or [list()] formatted for the
#' `Metadata` slot. See `Details`. Or an object of class [Slick].
#' @param Preset An optional named list. See `Details`.
#' @rdname Spider
#' @export
setMethod("Spider", c("missing", 'missing', 'missing'), function() newSpider())



#' @rdname Spider
#' @export
setMethod("Spider", c("data.frame", 'missing', 'missing'),
          function(Metadata) newSpider(Metadata))


#' @rdname Spider
#' @export
setMethod("Spider", c("list", 'missing', 'missing'),
          function(Metadata) newSpider(Metadata))


#' @rdname Spider
#' @export
setMethod("Spider", c("list", 'array', 'missing'),
          function(Metadata, Value) newSpider(Metadata, Value))

#' @rdname Spider
#' @export
setMethod("Spider", c("list", 'array', 'list'),
          function(Metadata, Value, Preset) newSpider(Metadata, Value, Preset))


#' @rdname Spider
#' @export
setMethod("Spider","Slick",function(Metadata) Metadata@Spider)


setGeneric("Spider<-", function(Slick, value) standardGeneric("Spider<-"))

#' @rdname Spider
#' @param Slick An object of class [Slick]
#' @param value An object of class [Spider]
#' @export
setMethod("Spider<-", "Slick", function(Slick, value) {
  Slick@Spider <- value
  methods::validObject(Slick)
  Slick
})




# ---- Table ----

#' @export
setGeneric("Table", function(object, lang, type) standardGeneric("Table"))

#' Create a DT::datatable table from information in Metadata slot
#'
#' Create a [DT::datatable] table from information in [Metadata] slot.
#'
#' @param object An object of class [Boxplot], [Kobe], [Quilt],
#' [Spider], [Timeseries], [Tradeoff], or [MPs] or [OMs]
#' @param lang Optional text string specifying the language (if available).
#' Either 'en', 'es', or 'fr' for English, Spanish, or French respectively
#'
#' @export
#' @rdname Table
setMethod("Table", c("Boxplot", 'character'), function(object, lang) {
  tableBoxplot(object, lang)
})



#' @export
#' @rdname Table
setMethod("Table",  c("Boxplot", 'missing'), function(object) {
  tableBoxplot(object, lang='en')
})

#' @export
#' @rdname Table
setMethod("Table", c("Kobe", 'character'), function(object, lang) {
  tableKobe(object, lang)
})

#' @export
#' @rdname Table
setMethod("Table",  c("Kobe", 'missing'), function(object) {
  tableKobe(object, lang='en')
})

#' @export
#' @rdname Table
setMethod("Table", c("Quilt", 'character'), function(object, lang) {
  tableQuilt(object, lang)
})

#' @export
#' @rdname Table
setMethod("Table",  c("Quilt", 'missing'), function(object) {
  tableQuilt(object, lang='en')
})

#' @export
#' @rdname Table
setMethod("Table", c("Spider", 'character'), function(object, lang) {
  tableSpider(object, lang)
})

#' @export
#' @rdname Table
setMethod("Table",  c("Spider", 'missing'), function(object) {
  tableSpider(object, lang='en')
})


#' @export
#' @rdname Table
setMethod("Table", c("Timeseries", 'character'), function(object, lang) {
  tableTimeseries(object, lang)
})

#' @export
#' @rdname Table
setMethod("Table",  c("Timeseries", 'missing'), function(object) {
  tableTimeseries(object, lang='en')
})

#' @export
#' @rdname Table
setMethod("Table", c("Tradeoff", 'character'), function(object, lang) {
  tableTradeoff(object, lang)
})

#' @export
#' @rdname Table
setMethod("Table",  c("Tradeoff", 'missing'), function(object) {
  tableTradeoff(object, lang='en')
})


#' @export
#' @rdname Table
setMethod("Table", c("MPs", 'character'), function(object, lang) {
  tableMPs(object, lang)
})

#' @export
#' @rdname Table
setMethod("Table",  c("MPs", 'missing'), function(object) {
  tableMPs(object, lang='en')
})

#' @export
#' @rdname Table
setMethod("Table", c("OMs", 'character'), function(object, lang) {
  tableOMs(object, lang)
})

#' @export
#' @rdname Table
setMethod("Table",  c("OMs", 'missing'), function(object) {
  tableOMs(object, lang='en')
})

#' @export
#' @param type For objects of class [OMs], create a table for the 'factor' or 'design' slot?
#' @rdname Table
setMethod("Table", c("OMs", 'character', 'character'), function(object, lang, type) {
  tableOMs(object, lang, type)
})


# ---- Time ----

#' @export
setGeneric("Time", function(object) standardGeneric("Time"))

#' Access or assign the Time slot in object of class `Kobe` or `Timeseries`
#'
#' Access or assign the Time slot in object of class [Kobe] or [Timeseries]
#' @rdname Time
#' @export
setMethod("Time", "Kobe", function(object) {object@Time})

#' @export
setMethod("Time", "Timeseries", function(object) {object@Time})


#' @export
setGeneric("Time<-", function(object, value) standardGeneric("Time<-"))
#' @export
#' @rdname Time
#' @param value A list or data.frame formatted for the class of `object`
setMethod("Time<-", "Kobe", function(object, value) {
  object@Time <- value
  methods::validObject(object)
  object
})

#' @export
#' @rdname Time
setMethod("Time<-", "Timeseries", function(object, value) {
  object@Time <- value
  methods::validObject(object)
  object
})


# ---- Timeseries ----


#' @export
setGeneric("Timeseries", function(Metadata, Time, Value, Preset) standardGeneric("Timeseries"))

#' @param Metadata An object of class [data.frame()] or [list()] formatted for the
#' `Metadata` slot. See `Details`. Or an object of class [Slick].
#' @param Time TODO
#' @param Value An `array` with the values for the `Timeseries` chart. See `Details`.
#' a character string specifying language (if available), or an object of class [MPs()]. See `Details`
#' @param Preset An optional named list. See `Details`.
#' @rdname Timeseries
setMethod("Timeseries", c("missing", 'missing', 'missing', 'missing'), function() newTimeseries())


#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("data.frame", 'missing', 'missing', 'missing'),
          function(Metadata) newTimeseries(Metadata))


#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("list", 'missing', 'missing'),
          function(Metadata) newTimeseries(Metadata))


#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("data.frame", 'data.frame'),
          function(Metadata, Time) newTimeseries(Metadata, Time))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("data.frame", 'list'),
          function(Metadata, Time) newTimeseries(Metadata, Time))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("list", 'data.frame'),
          function(Metadata, Time) newTimeseries(Metadata, Time))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("list", 'list'),
          function(Metadata, Time) newTimeseries(Metadata, Time))


#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("data.frame", 'data.frame', 'array'),
          function(Metadata, Time, Value) newTimeseries(Metadata, Time, Value))


#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("data.frame", 'list', 'array'),
          function(Metadata, Time, Value) newTimeseries(Metadata, Time, Value))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("list", 'data.frame', 'array'),
          function(Metadata, Time, Value) newTimeseries(Metadata, Time, Value))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("list", 'list', 'array'),
          function(Metadata, Time, Value) newTimeseries(Metadata, Time, Value))


#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("data.frame", 'list', 'array', 'list'),
          function(Metadata, Time, Value, Preset) newTimeseries(Metadata, Time, Value, Preset))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("list", 'data.frame', 'array', 'list'),
          function(Metadata, Time, Value, Preset) newTimeseries(Metadata, Time, Value, Preset))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("list", 'list', 'array', 'list'),
          function(Metadata, Time, Value, Preset) newTimeseries(Metadata, Time, Value, Preset))

#' @rdname Timeseries
#' @export
setMethod("Timeseries", c("data.frame", 'data.frame', 'array', 'list'),
          function(Metadata, Time, Value, Preset) newTimeseries(Metadata, Time, Value, Preset))

#' @rdname Timeseries
#' @export
setMethod("Timeseries","Slick",function(Metadata) Metadata@Timeseries)


setGeneric("Timeseries<-", function(Slick, value) standardGeneric("Timeseries<-"))

#' @rdname Timeseries
#' @param Slick An object of class [Slick]
#' @param value An object of class [Timeseries]
#' @export
setMethod("Timeseries<-", "Slick", function(Slick, value) {
  Slick@Timeseries <- value
  methods::validObject(Slick)
  Slick
})




# ---- Tradeoff ----

setGeneric("Tradeoff", function(Metadata, Value, Preset, Selected) standardGeneric("Tradeoff"))


#' @param Metadata An object of class [data.frame()] or [list()] formatted for the
#' `Metadata` slot. See `Details`. Or an object of class [Slick].
#' @param Value An `array` with the values for the `Boxplot` chart. See `Details`.
#' @param Preset An optional named list. See `Details`.
#' @param Selected TODO
#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("missing", 'missing', 'missing', 'missing'), function() newTradeoff())


#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("data.frame", 'missing', 'missing', 'missing'),
          function(Metadata) newTradeoff(Metadata))

#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("data.frame", 'array', 'missing', 'missing'),
          function(Metadata, Value) newTradeoff(Metadata, Value))

#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("data.frame", 'array', 'list', 'missing'),
          function(Metadata, Value, Preset) newTradeoff(Metadata, Value, Preset))

#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("data.frame", 'array', 'list', 'character'),
          function(Metadata, Value, Preset, Selected) newTradeoff(Metadata, Value, Preset, Selected))

#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("data.frame", 'array', 'list', 'numeric'),
          function(Metadata, Value, Preset, Selected) newTradeoff(Metadata, Value, Preset, Selected))


#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("list", 'missing', 'missing', 'missing'),
          function(Metadata) newTradeoff(Metadata))

#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("list", 'array', 'missing', 'missing'),
          function(Metadata, Value) newTradeoff(Metadata, Value))

#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("list", 'array', 'list', 'missing'),
          function(Metadata, Value, Preset) newTradeoff(Metadata, Value, Preset))

#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("list", 'array', 'list', 'character'),
          function(Metadata, Value, Preset, Selected) newTradeoff(Metadata, Value, Preset, Selected))


#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", c("list", 'array', 'list', 'numeric'),
          function(Metadata, Value, Preset, Selected) newTradeoff(Metadata, Value, Preset, Selected))


#' @rdname Tradeoff
#' @export
setMethod("Tradeoff", "Slick", function(Metadata) {
  Metadata@Tradeoff
})


setGeneric("Tradeoff<-", function(Slick, value) standardGeneric("Tradeoff<-"))

#' @rdname Tradeoff
#' @param An object of class [Slick]
#' @param An object of class [Tradeoff]
#' @export
setMethod("Tradeoff<-", "Slick", function(Slick, value) {
  Slick@Tradeoff <- value
  methods::validObject(Slick)
  Slick
})

# ---- Value ----
setGeneric("Value", function(object) standardGeneric("Value"))

#' Assign or access the `Value` slot
#' @param object An object of class [Boxplot], [Kobe], [Quilt],
#' [Spider], [Timeseries], or [Tradeoff]
#' @export
#' @rdname Value
setMethod("Value", 'Boxplot', function(object) {
  object@Value
})

#' @export
#' @rdname Value
setMethod("Value", 'Kobe', function(object) {
  object@Value
})

#' @export
#' @rdname Value
setMethod("Value", 'Quilt', function(object) {
  object@Value
})

#' @export
#' @rdname Value
setMethod("Value", 'Spider', function(object) {
  object@Value
})

#' @export
#' @rdname Value
setMethod("Value", 'Timeseries', function(object) {
  object@Value
})

#' @export
#' @rdname Value
setMethod("Value", 'Tradeoff', function(object) {
  object@Value
})


setGeneric("Value<-", function(object, value) standardGeneric("Value<-"))

#' @param value An [array], formatted to match the class of `object`. See the documentation for
#' corresponding `object` class for more details.
#' @rdname Value
#' @export
setMethod("Value<-", "Boxplot", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})


#' @rdname Value
#' @export
setMethod("Value<-", "Kobe", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

#' @rdname Value
#' @export
setMethod("Value<-", "Quilt", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

#' @rdname Value
#' @export
setMethod("Value<-", "Spider", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

#' @rdname Value
#' @export
setMethod("Value<-", "Timeseries", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

#' @rdname Value
#' @export
setMethod("Value<-", "Tradeoff", function(object, value) {
  if (is.null(value)) return(object)
  object@Value <- value
  methods::validObject(object)
  object
})

# ---- Slick ----


# ---- show ----

#' @export
setMethod("show", "MPs", function(object) {
  showMPs(object)
})


#' @export
setMethod("show", "Slick", function(object) {
  showSlick(object)
})



# ---- plot ----

#' @export
setMethod("plot", "Quilt", function(x, ...) {
 plotQuilt(x, ...)
})







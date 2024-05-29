
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

#' @export
setGeneric("MPs", function(x, ...) standardGeneric("MPs"))


#' @describeIn MPs Create a new object of class [MPs()]
#' @usage MPs() # Create an empty MPs() object
#' @export
setMethod("MPs","missingOrNULL",function(x, ...) newMPs(...))

#' @describeIn MPs Create a new object of class [MPs()]
#' @usage MPs(Metadata=data.frame(), Preset=list()) # Populate a MPs() object
#' @param Metadata A [data.frame()] with a specific structure. See `Details`
#' @param Preset An optional [list()] with a specific structure. See `Details`
#' @export
setMethod("MPs","dataframe_list",function(x, ...) newMPs(x, ...))


#' @describeIn MPs Return the [MPs()] object from an object of class [Slick()]
#' @param lang Optional. Character string length 2 to specify language (if available).  'en', 'es', or 'fr'
#' @usage MPs(Slick, lang='en') # return MPs slot from Slick object
#' @export
setMethod("MPs", "Slick", function(x, lang=NULL) {
  get_language(x@MPs, lang)
} )


#' @export
setGeneric("MPs<-", function(Slick, value) standardGeneric("MPs<-"))

#' @describeIn MPs Assign an object of class [MPs()] to an object of class [Slick()]
#' @param Slick An object of class [Slick()]
#' @usage MPs(Slick) <- MPs() # assign an MPs object to the MPs slot in Slick object
#' @export
setMethod("MPs<-", "Slick", function(Slick, value) {
  Slick@MPs <- value
  methods::validObject(Slick)
  Slick
})


# ---- setGenerics -----

setGeneric("Author", function(x) standardGeneric("Author"))
setGeneric("Author<-", function(x, value) standardGeneric("Author<-"))

setGeneric("Colors", function(x) standardGeneric("Colors"))
setGeneric("Colors<-", function(x, value) standardGeneric("Colors<-"))

setGeneric("Date", function(x) standardGeneric("Date"))
setGeneric("Date<-", function(x, value) standardGeneric("Date<-"))

setGeneric("Design", function(x, ...) standardGeneric("Design"))
setGeneric("Design<-", function(x, value) standardGeneric("Design<-"))

setGeneric("Email", function(x) standardGeneric("Email"))
setGeneric("Email<-", function(x, value) standardGeneric("Email<-"))

setGeneric("Institution", function(x) standardGeneric("Institution"))
setGeneric("Institution<-", function(x, value) standardGeneric("Institution<-"))

setGeneric("Introduction", function(x, ...) standardGeneric("Introduction"))
setGeneric("Introduction<-", function(x, value) standardGeneric("Introduction<-"))

#' Access or modify the `Metadata` slot
#'
#' @param x An object with a `Metadata` slot
#' @param ... Additional arguments, specific to the class of object `x`
#'
setGeneric("Metadata", function(x, ...) standardGeneric("Metadata"))

setGeneric("Metadata<-", function(x, value) standardGeneric("Metadata<-"))


# #' @param x Either an object of class [Slick()], class [data.frame()] or NULL (empty)
# #' @param ... Extra named arguments passed to [MPs()]
# #' @rdname MPs
# #' @export
# setGeneric("MPs", function(x, ...) standardGeneric("MPs"))

# #' @rdname MPs
# #' @export
# setGeneric("MPs<-", function(x, value) standardGeneric("MPs<-"))

setGeneric("OMs", function(x, ...) standardGeneric("OMs"))
setGeneric("OMs<-", function(x, value) standardGeneric("OMs<-"))

#' Access or modify the `Metadata` slot
#'
#' @param x An object with a `Preset` slot
#' @param ... Additional arguments, specific to the class of object `x`
#'
setGeneric("Preset", function(x) standardGeneric("Preset"))
setGeneric("Preset<-", function(x, value) standardGeneric("Preset<-"))

setGeneric("Quilt", function(x, ...) standardGeneric("Quilt"))
setGeneric("Quilt<-", function(x, value) standardGeneric("Quilt<-"))

setGeneric("Selected", function(x, ...) standardGeneric("Selected"))
setGeneric("Selected<-", function(x, value) standardGeneric("Selected<-"))

setGeneric("Spider", function(x, ...) standardGeneric("Spider"))
setGeneric("Spider<-", function(x, value) standardGeneric("Spider<-"))

setGeneric("Subtitle", function(x, ...) standardGeneric("Subtitle"))
setGeneric("Subtitle<-", function(x, value) standardGeneric("Subtitle<-"))

setGeneric("Table", function(x, ...) standardGeneric("Table"))

setGeneric("Time", function(x, ...) standardGeneric("Time"))
setGeneric("Time<-", function(x, value) standardGeneric("Time<-"))

setGeneric("TimeLab", function(x, ...) standardGeneric("TimeLab"))
setGeneric("TimeLab<-", function(x, value) standardGeneric("TimeLab<-"))


setGeneric("Timeseries", function(x, ...) standardGeneric("Timeseries"))
setGeneric("Timeseries<-", function(x, value) standardGeneric("Timeseries<-"))

setGeneric("Title", function(x, ...) standardGeneric("Title"))
setGeneric("Title<-", function(x, value) standardGeneric("Title<-"))

setGeneric("Tradeoff", function(x, ...) standardGeneric("Tradeoff"))
setGeneric("Tradeoff<-", function(x, value) standardGeneric("Tradeoff<-"))

setGeneric("Value", function(x, ...) standardGeneric("Value"))
setGeneric("Value<-", function(x, value) standardGeneric("Value<-"))


# ---- setMethods -----



#' @export
setMethod("Metadata<-", "Boxplot", function(x, value) {
  x@Metadata <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Table", "Boxplot", function(x, lang=NULL) {
  tableBoxplot(x, lang)
})






## Metadata ----
#' @describeIn Metadata Modify the `Metadata` slot in an [MPs()] object
#' @param lang Character string to select language.
#' Either 'en', 'es', or 'fr' for English, Spanish, or French respectively.
#' If selected language isn't available in the object, it defaults to first language
#' @export
setMethod("Metadata", "MPs", function(x, lang='en') {
  get_language(x@Metadata, lang)
})

#' @describeIn Metadata Assign the `Metadata` slot in an [MPs()] object
#' @param value A correctly structured data.frame for object class `x`
#' @export
setMethod("Metadata<-", "MPs", function(x, value) {
  x@Metadata <- value
  methods::validObject(x)
  x
})

## Preset ----

#' @describeIn Preset Modify the `Preset` slot in an [MPs()] object
#' @export
setMethod("Preset", "MPs", function(x) {
  x@Preset
})

#' @describeIn Preset Assign the `Preset` slot in an [MPs()] object
#' @param value A correctly structured named list for object class `x`
#' @export
setMethod("Preset<-", "MPs", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("show", "MPs", function(object) {
  showMPs(object)
})


#' @describeIn Table Generate a `DT::datatable` object for objects of class `MPs`
#' @export
setMethod("Table", "MPs", function(x, lang=NULL) {
  tableMPs(x, lang)
})



## OMs ----

#' @export
setMethod("Design", "OMs", function(x) {x@Design})

#' @export
setMethod("Design<-", "OMs", function(x, value) {
  if (!inherits(value, 'data.frame')) {
    stop('`Design` must be class `data.frame`', call.=FALSE)
  }

  x@Design <- value
  methods::validObject(x)
  x
})

#' @noRd
#' @export
setMethod("OMs","missingOrNULL",function(x, ...) newOMs(...))

#' @noRd
#' @export
setMethod("OMs","dataframe_list",function(x, ...) newOMs(x, ...))

#' @describeIn Metadata Modify the `Metadata` slot in an [OMs()] object
#' @export
setMethod("Metadata", "OMs", function(x, lang='en') {
  get_language(x@Metadata, lang)
})

#' @describeIn Metadata Assign the `Metadata` slot in an [OMs()] object
#' @export
setMethod("Metadata<-", "OMs", function(x, value) {
  x@Metadata <- value
  methods::validObject(x)
  x
})



#' @describeIn Preset Modify the `Preset` slot in an [OMs()] object
#' @export
setMethod("Preset", "OMs", function(x) {
  x@Preset
})

#' @describeIn Preset Assign the `Preset` slot in an [OMs()] object
#' @export
setMethod("Preset<-", "OMs", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})


#' @export
setMethod("Table", "OMs", function(x, lang=NULL, type=NULL) {
  tableOMs(x, lang, type)
})



## Boxplot ----

setGeneric("Boxplot", function(x, ...) standardGeneric("Boxplot"))
setGeneric("Boxplot<-", function(x, value) standardGeneric("Boxplot<-"))

#' @export
setMethod("Boxplot","character_list",function(x, ...) newBoxplot(x, ...))

#' @export
setMethod("Boxplot","missingOrNULL",function(x, ...) newBoxplot(...))

#' @export
setMethod("Boxplot","character",function(x, ...) newBoxplot(x, ...))



#' @export
setMethod("Boxplot<-", "Slick", function(x, value) {
  x@Boxplot <- value
  x
})

#' @export
setMethod("Metadata", "Boxplot", function(x, lang=NULL) {
  get_language(x@Metadata, lang)
})

#' @export
setMethod("Metadata<-", "Boxplot", function(x, value) {
  x@Metadata <- value
  x
})

#' @export
setMethod("Preset", "Boxplot", function(x) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Boxplot", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Table", "Boxplot", function(x, lang=NULL) {
  tableBoxplot(x, lang)
})


#' @export
setMethod("Value", "Boxplot", function(x) {x@Value})

#' @export
setMethod("Value<-", "Boxplot", function(x, value) {
  x@Value <- value
  methods::validObject(x)
  x
})



## Kobe ----

setGeneric("Kobe", function(x, ...) standardGeneric("Kobe"))
setGeneric("Kobe<-", function(x, value) standardGeneric("Kobe<-"))

#' @export
setMethod("Kobe","character_list",function(x, ...) newKobe(x, ...))

#' @export
setMethod("Kobe","missingOrNULL",function(x, ...) newKobe(...))

#' @export
setMethod("Kobe","character",function(x, ...) newKobe(x, ...))


#' @export
setMethod("Metadata", "Kobe", function(x, lang=NULL) {
  get_language(x@Metadata, lang)
})

#' @export
setMethod("Metadata<-", "Kobe", function(x, value) {
  x@Metadata <- value
  x
})

#' @export
setMethod("Preset", "Kobe", function(x) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Kobe", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Table", "Kobe", function(x, lang=NULL) {
  tableKobe(x, lang)
})


#' @export
setMethod("Time", "Kobe", function(x) {x@Time})

#' @export
setMethod("Time<-", "Kobe", function(x, value) {
  x@Time <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Value", "Kobe", function(x) {x@Value})

#' @export
setMethod("Value<-", "Kobe", function(x, value) {
  x@Value <- value
  methods::validObject(x)
  x
})



## Quilt ----

#' @export
setMethod("Quilt","character_list",function(x, ...) newQuilt(x, ...))

#' @export
setMethod("Quilt","missingOrNULL",function(x, ...) newQuilt(...))

#' @export
setMethod("Quilt","character",function(x, ...) newQuilt(x, ...))



#' @export
setMethod("Colors", "Quilt", function(x) {x@Colors})

#' @export
setMethod("Colors<-", "Quilt", function(x, value) {
  x@Colors <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Metadata", "Quilt", function(x, lang=NULL) {
  get_language(x@Metadata, lang)

})

#' @export
setMethod("Metadata<-", "Quilt", function(x, value) {
  x@Metadata <- value
  x
})

#' @export
setMethod("plot", "Quilt", function(x, ...) {
 plotQuilt(x, ...)
})

#' @export
setMethod("Preset", "Quilt", function(x) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Quilt", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Table", "Quilt", function(x, lang=NULL) {
  tableQuilt(x, lang)
})

#' @export
setMethod("Value", "Quilt", function(x) {x@Value})

#' @export
setMethod("Value<-", "Quilt", function(x, value) {
  x@Value <- value
  methods::validObject(x)
  x
})



## Slick ----

#' @export
setMethod("Author", "Slick", function(x) {
  x@Author
})

#' @export
setMethod("Author<-", "Slick", function(x, value) {
  if (is.null(value)) return(x)
  x@Author <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Boxplot","Slick",function(x) x@Boxplot)

#' @export
setMethod("Boxplot<-", "Slick", function(x, value) {
  x@Boxplot <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Date", "Slick", function(x) {x@Date})

#' @export
setMethod("Date<-", "Slick", function(x, value) {
  if (is.null(value)) return(x)
  if (inherits(value, 'POSIXct'))
    value <- value |> as.Date() |> as.character()
  x@Date <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Design", "Slick", function(x) {x@OMs@Design})

#' @export
setMethod("Design<-", "Slick", function(x, value) {
  x@OMs@Design <- value
  methods::validObject(x)
  x
})


#' @export
setMethod("Email", "Slick", function(x) {
  x@Email
})

#' @export
setMethod("Email<-", "Slick", function(x, value) {
  if (is.null(value)) return(x)
  x@Email <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Institution", "Slick", function(x) {
  x@Institution
})

#' @export
setMethod("Institution<-", "Slick", function(x, value) {
  if (is.null(value)) return(x)
  x@Institution <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Introduction", "Slick", function(x, lang=NULL) {
  get_language(x@Introduction, lang)
})

#' @export
setMethod("Introduction<-", "Slick", function(x, value) {
  if (is.null(value)) return(x)
  x@Introduction <- value
  methods::validObject(x)
  x
})


#' @export
setMethod("Kobe", "Slick", function(x) {
  x@Kobe
})

#' @export
setMethod("Kobe<-", "Slick", function(x, value) {
  x@Kobe <- value
  methods::validObject(x)
  x
})


#' assign an OMs object
#' @export
setMethod("OMs<-", "Slick", function(x, value) {
  x@OMs <- value
  methods::validObject(x)
  x
})

#' print an OMs object
#' @export
setMethod("OMs", "Slick", function(x) {
  x@OMs
})


#' @export
setMethod("Quilt","Slick",function(x) x@Quilt)

#' @export
setMethod("Quilt<-", "Slick", function(x, value) {
  x@Quilt <- value
  x
})


#' @export
setMethod("Spider<-", "Slick", function(x, value) {
  x@Spider <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Spider", "Slick", function(x) {
  x@Spider
})


#' @export
setMethod("Subtitle", "Slick", function(x, lang=NULL) {
  get_language(x@Subtitle, lang)
})

#' @export
setMethod("Subtitle<-", "Slick", function(x, value) {
  x@Subtitle <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("show", "Slick", function(object) {
  showSlick(object)
})

#' @export
setMethod("Title", "Slick", function(x, lang=NULL) {
  get_language(x@Title, lang)
})

#' @export
setMethod("Title<-", "Slick", function(x, value) {
  x@Title <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Timeseries","Slick",function(x) x@Timeseries)

#' @export
setMethod("Timeseries<-", "Slick", function(x, value) {
  x@Timeseries <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Tradeoff", "Slick", function(x) {
  x@Tradeoff
})

#' @export
setMethod("Tradeoff<-", "Slick", function(x, value) {
  x@Tradeoff <- value
  methods::validObject(x)
  x
})
## Spider ----

#' @export
setMethod("Spider","character_list",function(x, ...) newSpider(x, ...))

#' @export
setMethod("Spider","missingOrNULL",function(x, ...) newSpider(...))

#' @export
setMethod("Spider","character",function(x, ...) newSpider(x, ...))


#' @export
setMethod("Spider","Slick",function(x) x@Spider)

#' @export
setMethod("Spider<-", "Slick", function(x, value) {
  x@Spider <- value
  methods::validObject(x)
  x
})


#' @export
setMethod("Metadata", "Spider", function(x, lang=NULL) {
  get_language(x@Metadata, lang)

})

#' @export
setMethod("Metadata<-", "Spider", function(x, value) {
  x@Metadata <- value
  x
})

#' @export
setMethod("Preset", "Spider", function(x) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Spider", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Table", "Spider", function(x, lang=NULL) {
  tableSpider(x, lang)
})

#' @export
setMethod("Value", "Spider", function(x) {
  x@Value
})


#' @export
setMethod("Value<-", "Spider", function(x, value) {
  x@Value <- value
  methods::validObject(x)
  x
})



## Timeseries ----

#' @export
setMethod("Timeseries","character_list",function(x, ...) newTimeseries(x, ...))

#' @export
setMethod("Timeseries","missingOrNULL",function(x, ...) newTimeseries(...))

#' @export
setMethod("Timeseries","character",function(x, ...) newTimeseries(x, ...))

#' @export
setMethod("Timeseries","Slick",function(x) x@Timeseries)

#' @export
setMethod("Timeseries<-", "Slick", function(x, value) {
  x@Timeseries <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Metadata", "Timeseries", function(x, lang=NULL) {
  get_language(x@Metadata, lang)
})


#' @export
setMethod("Preset", "Timeseries", function(x) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Timeseries", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Table", "Timeseries", function(x, lang=NULL) {
  tableTimeseries(x, lang)
})

#' @export
setMethod("Time", "Timeseries", function(x) {x@Time})

#' @export
setMethod("Time<-", "Timeseries", function(x, value) {
  x@Time <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Value", "Timeseries", function(x) {x@Value})

#' @export
setMethod("Value<-", "Timeseries", function(x, value) {
  x@Value <- value
  methods::validObject(x)
  x
})




## Tradeoff ----

#' @export
setMethod("Tradeoff","character_list",function(x, ...) newTradeoff(x, ...))

#' @export
setMethod("Tradeoff","missingOrNULL",function(x, ...) newTradeoff(...))

#' @export
setMethod("Tradeoff","character",function(x, ...) newTradeoff(x, ...))


#' @export
setMethod("Metadata", "Tradeoff", function(x, lang=NULL) {
  get_language(x@Metadata, lang)
})

#' @export
setMethod("Preset", "Tradeoff", function(x) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Tradeoff", function(x, value) {
  x@Preset <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Selected", "Tradeoff", function(x) {
  x@Selected
})

#' @export
setMethod("Selected<-", "Tradeoff", function(x, value) {
  x@Selected <- value
  methods::validObject(x)
  x
})

#' @export
setMethod("Table", "Tradeoff", function(x, lang=NULL) {
  tableTradeoff(x, lang)
})

#' @export
setMethod("Value", "Tradeoff", function(x) {
  x@Value
})

#' @export
setMethod("Value<-", "Tradeoff", function(x, value) {
  x@Value <- value
  methods::validObject(x)
  x
})




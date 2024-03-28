get_language <- function(value, lang) {
  if (is.null(lang))
    return(value)

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


#' @include class_Boxplot.R
#' @include class_Kobe.R
#' @include class_MPs.R
#' @include class_OMs.R
#' @include class_Quilt.R
#' @include class_SlickData.R
#' @include class_Spider.R
#' @include class_Timeseries.R
NULL




# setMethod("show", "OMs",
#           function(object) {
#             print('An object of class `OMs`')
#
#             })


# Design <- expand.grid(M=c(0.1, 0.2, 0.3), h=c(0.7,0.9))
# Description <- list(c('Natural mortality (M) = 0.1',
#                       'Natural mortality (M) = 0.2',
#                       'Natural mortality (M) = 0.3'),
#                     c('Steepness (h) = 0.7',
#                       'Steepness (h) = 0.9'))
# Label <- list(c(M=0.1, M=0.2, M=0.3),
#                c(h=0.7, h=0.9))
# myOMs <- OMs(Design, Description, Label)
# myOMs


## OMs ----
#' @rdname OMs
setGeneric("OMs", function(x, ...) standardGeneric("OMs"))

#' @rdname OMs
setGeneric("OMs<-", function(x, value) standardGeneric("OMs<-"))



#' @rdname OMs
#' @export
setMethod("OMs","data.frame",function(x, ...) newOMs(x, ...))

#' @rdname OMs
#' @export
setMethod("OMs","missingOrNULL",function(x, ...) newOMs(...))

#' @rdname OMs
#' @export
setMethod("OMs", "SlickData", function(x) {
  x@OMs
})

#' @rdname OMs
#' @export
setMethod("OMs<-", "SlickData", function(x, value) {
  x@OMs <- value
  x
})

## MPs ----

setGeneric("MPs", function(x, ...) standardGeneric("MPs"))
setGeneric("MPs<-", function(x, value) standardGeneric("MPs<-"))
setMethod("MPs","character_list",function(x, ...) newMPs(x, ...))

#' @export
setMethod("MPs","missingOrNULL",function(x, ...) newMPs(...))

#' @export
setMethod("MPs","character",function(x, ...) newMPs(x, ...))

#' @export
setMethod("MPs", "SlickData", function(x) x@MPs)

#' @export
setMethod("MPs<-", "SlickData", function(x, value) {
  x@MPs <- value
  x
})

## Link ----
setGeneric("Link", function(MPs) standardGeneric("Link"))
setGeneric("Link<-", function(MPs, value) standardGeneric("Link<-"))

#' @export
setMethod("Link","MPs",function(MPs) {
  MPs@Link
})

#' @export
setMethod("Link<-","MPs",function(MPs, value) {
  MPs@Link <- value
  MPs
})



## Design ----
setGeneric("Design", function(x, ...) standardGeneric("Design"))
setGeneric("Design<-", function(x, value) standardGeneric("Design<-"))

#' @export
setMethod("Design", "OMs", function(x) {x@Design})

#' @export
setMethod("Design<-", "OMs", function(x, value) {
  if (!inherits(value, 'data.frame')) {
    stop('`Design` must be class `data.frame`', call.=FALSE)
  }

  x@Design <- value
  validObject(x)
  x
})

#' @export
setMethod("Design", "SlickData", function(x) {
  x@OMs@Design
})

#' @export
setMethod("Design<-", "SlickData", function(x, value) {
  x@OMs@Design <- value
  x
})

# Color ----
setGeneric("Color", function(x) standardGeneric("Color"))
setGeneric("Color<-", function(x, value) standardGeneric("Color<-"))

#' @export
setMethod("Color", "Quilt", function(x) {
  x@Color
})

#' @export
setMethod("Color<-", "Quilt", function(x, value) {
  x@Color <- value
  x
})



# Description  -----
setGeneric("Description", function(x, lang=NULL) standardGeneric("Description"))
setGeneric("Description<-", function(x, value) standardGeneric("Description<-"))


#' @export
setMethod("Description", "OMs", function(x, lang=NULL) {
  get_language(x@Description, lang)
})

#' @export
setMethod("Description<-", "OMs", function(x, value) {
  if (!inherits(value, 'list')) {
    stop('`Description` must be class `list`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})

#' @export
setMethod("Description", "MPs", function(x, lang=NULL) {
  get_language(x@Description, lang)
})


#' @export
setMethod("Description<-", "MPs", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Description` must be class `character`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})

#' @export
setMethod("Description", "Quilt", function(x, lang=NULL) {
  get_language(x@Description, lang)
})


#' @export
setMethod("Description<-", "Quilt", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Description` must be class `character`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})

#' @export
setMethod("Description", "Spider", function(x, lang=NULL) {
  get_language(x@Description, lang)
})

#' @export
setMethod("Description<-", "Spider", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Description` must be class `character`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})


#' @export
setMethod("Description", "Boxplot", function(x, lang=NULL) {
  get_language(x@Description, lang)
})

#' @describeIn newBoxplot Assigns Description for an object of class `Boxplot`
#' @export
setMethod("Description<-", "Boxplot", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Description` must be class `character`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})


#' @describeIn newKobe Returns Description of an object of class `Kobe`
#' @export
setMethod("Description", "Kobe", function(x, lang=NULL) {
  get_language(x@Description, lang)
})

#' @describeIn newKobe Assigns Description for an object of class `Kobe`
#' @export
setMethod("Description<-", "Kobe", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Description` must be class `character`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})


#' @export
setMethod("Description", "Timeseries", function(x, lang=NULL) {
  get_language(x@Description, lang)
})

#' @export
setMethod("Description<-", "Timeseries", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Description` must be class `character`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})


# Label  -----
setGeneric("Label", function(x, lang=NULL) standardGeneric("Label"))
setGeneric("Label<-", function(x, value) standardGeneric("Label<-"))


#' @export
setMethod("Label", "OMs", function(x, lang=NULL) {
  get_language(x@Label, lang)
})


#' @export
setMethod("Label<-", "OMs", function(x, value) {
  if (!inherits(value, 'list')) {
    stop('`Label` must be class `list`', call.=FALSE)
  }

  x@Label <- value
  validObject(x)
  x
})

#' @describeIn newMPs Returns Label of an object of class `MPs`
#' @export
setMethod("Label", "MPs", function(x, lang=NULL) {
  get_language(x@Label, lang)
})


#' @describeIn newMPs Assigns Label for an object of class `MPs`
#' @export
setMethod("Label<-", "MPs", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Label` must be class `character`', call.=FALSE)
  }
  x@Label <- value
  validObject(x)
  x
})

#' @export
setMethod("Label", "Quilt", function(x, lang=NULL) {
  get_language(x@Label, lang)
})

#' @export
setMethod("Label<-", "Quilt", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Label` must be class `character`', call.=FALSE)
  }
  x@Label <- value
  validObject(x)
  x
})

#' @describeIn newSpider Returns Label of an object of class `Spider`
#' @export
setMethod("Label", "Spider", function(x, lang=NULL) {
  get_language(x@Label, lang)
})


#' @describeIn newSpider Assigns Label for an object of class `Spider`
#' @export
setMethod("Label<-", "Spider", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Label` must be class `character`', call.=FALSE)
  }
  x@Label <- value
  validObject(x)
  x
})

#' @describeIn newBoxplot Returns Label of an object of class `Boxplot`
#' @export
setMethod("Label", "Boxplot", function(x, lang=NULL) {
  get_language(x@Label, lang)
})


#' @describeIn newBoxplot Assigns Label for an object of class `Boxplot`
#' @export
setMethod("Label<-", "Boxplot", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Label` must be class `character`', call.=FALSE)
  }
  x@Label <- value
  validObject(x)
  x
})

#' @describeIn newKobe Returns Label of an object of class `Kobe`
#' @export
setMethod("Label", "Kobe", function(x, lang=NULL) {
  get_language(x@Label, lang)
})


#' @describeIn newKobe Assigns Label for an object of class `Kobe`
#' @export
setMethod("Label<-", "Kobe", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Label` must be class `character`', call.=FALSE)
  }
  x@Label <- value
  validObject(x)
  x
})

#' @describeIn newTimeSeries Returns Label of an object of class `Timeseries`
#' @export
setMethod("Label", "Timeseries", function(x, lang=NULL) {
  get_language(x@Label, lang)
})

#' @describeIn newTimeSeries Assigns Label for an object of class `Timeseries`
#' @export
setMethod("Label<-", "Timeseries", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Label` must be class `character`', call.=FALSE)
  }
  x@Label <- value
  validObject(x)
  x
})

## Value ----

setGeneric("Value", function(x, ...) standardGeneric("Value"))
setGeneric("Value<-", function(x, value) standardGeneric("Value<-"))

#' @export
setMethod("Value", "Quilt", function(x) {x@Value})

#' @export
setMethod("Value<-", "Quilt", function(x, value) {
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Spider", function(x) {x@Value})

#' @export
setMethod("Value<-", "Spider", function(x, value) {
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Boxplot", function(x) {x@Value})

#' @export
setMethod("Value<-", "Boxplot", function(x, value) {
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Kobe", function(x) {x@Value})

#' @export
setMethod("Value<-", "Kobe", function(x, value) {
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Timeseries", function(x) {x@Value})

#' @export
setMethod("Value<-", "Timeseries", function(x, value) {
  x@Value <- value
  validObject(x)
  x
})

## Default ----

setGeneric("Default", function(x, ...) standardGeneric("Default"))
setGeneric("Default<-", function(x, value) standardGeneric("Default<-"))

#' @export
setMethod("Default", "OMs", function(x) {x@Default})

#' @export
setMethod("Default<-", "OMs", function(x, value) {

  # if (!inherits(value, 'numeric')) {
  #   stop('`Default` must be class `numeric`', call.=FALSE)
  # }

  x@Default <- value
  validObject(x)
  x
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
setMethod("Boxplot","SlickData",function(x, ...) x@Boxplot)

#' @export
setMethod("Boxplot", "SlickData", function(x) {
  x@Boxplot
})

#' @export
setMethod("Boxplot<-", "SlickData", function(x, value) {
  x@Boxplot <- value
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
setMethod("Kobe","SlickData",function(x, ...) x@Kobe)

#' @export
setMethod("Kobe", "SlickData", function(x) {
  x@Kobe
})

#' @export
setMethod("Kobe<-", "SlickData", function(x, value) {
  x@Kobe <- value
  x
})
## Quilt ----

setGeneric("Quilt", function(x, ...) standardGeneric("Quilt"))
setGeneric("Quilt<-", function(x, value) standardGeneric("Quilt<-"))

#' @export
setMethod("Quilt","character_list",function(x, ...) newQuilt(x, ...))

#' @export
setMethod("Quilt","missingOrNULL",function(x, ...) newQuilt(...))

#' @export
setMethod("Quilt","character",function(x, ...) newQuilt(x, ...))

#' @export
setMethod("Quilt","SlickData",function(x, ...) x@Quilt)

#' @export
setMethod("Quilt<-", "SlickData", function(x, value) {
  x@Quilt <- value
  x
})

## Spider ----

setGeneric("Spider", function(x, ...) standardGeneric("Spider"))
setGeneric("Spider<-", function(x, value) standardGeneric("Spider<-"))

#' @export
setMethod("Spider","character_list",function(x, ...) newSpider(x, ...))

#' @export
setMethod("Spider","missingOrNULL",function(x, ...) newSpider(...))

#' @export
setMethod("Spider","character",function(x, ...) newSpider(x, ...))

#' @export
setMethod("Spider","SlickData",function(x, ...) x@Spider)

#' @export
setMethod("Spider", "SlickData", function(x) {
  x@Spider
})

#' @export
setMethod("Spider<-", "SlickData", function(x, value) {
  x@Spider <- value
  x
})
## Timeseries ----

setGeneric("Timeseries", function(x, ...) standardGeneric("Timeseries"))
setGeneric("Timeseries<-", function(x, value) standardGeneric("Timeseries<-"))

#' @export
setMethod("Timeseries","character_list",function(x, ...) newTimeseries(x, ...))

#' @export
setMethod("Timeseries","missingOrNULL",function(x, ...) newTimeseries(...))

#' @export
setMethod("Timeseries","character",function(x, ...) newTimeseries(x, ...))

#' @export
setMethod("Timeseries","SlickData",function(x, ...) x@Timeseries)

#' @export
setMethod("Timeseries", "SlickData", function(x) {
  x@Timeseries
})

#' @export
setMethod("Timeseries<-", "SlickData", function(x, value) {
  x@Timeseries <- value
  x
})

## SlickData ----

## ---- SlickData Generics ----





#' @export
setGeneric("Title", function(SlickData, ...) standardGeneric("Title"))


setGeneric("Title<-", function(SlickData, value) standardGeneric("Title<-"))

#' @param SlickData An object of class `SlickData`
#' @param lang two letter character string specifying the language
#' to return (if available). Currently supported languages are: `en`, `es`, `fr`
#' @export
#' @rdname SlickData
setMethod("Title", "SlickData", function(SlickData, lang=NULL) {
  get_language(SlickData@Title, lang)
})


#' @param value Value to replace.
#' @export
#' @rdname SlickData
setMethod("Title<-", "SlickData", function(SlickData, value) {
  SlickData@Title <- value
  SlickData
})



setGeneric("Subtitle", function(SlickData, ...) standardGeneric("Subtitle"))
setGeneric("Subtitle<-", function(SlickData, value) standardGeneric("Subtitle<-"))


#' @export
#' @rdname SlickData
setMethod("Subtitle", "SlickData", function(SlickData, lang=NULL) {
  get_language(SlickData@Subtitle, lang)
})


#' @export
#' @rdname SlickData
setMethod("Subtitle<-", "SlickData", function(SlickData, value) {
  SlickData@Subtitle <- value
  SlickData
})


setGeneric("Fishery", function(SlickData, ...) standardGeneric("Fishery"))
setGeneric("Fishery<-", function(SlickData, value) standardGeneric("Fishery<-"))

#' @export
setMethod("Fishery", "SlickData", function(SlickData, lang=NULL) {
  get_language(SlickData@Fishery, lang)
  })

#' @export
setMethod("Fishery<-", "SlickData", function(SlickData, value) {
  if (is.null(value)) return(SlickData)
  SlickData@Fishery <- value
  SlickData
})

setGeneric("Introduction", function(SlickData, ...) standardGeneric("Introduction"))
setGeneric("Introduction<-", function(SlickData, value) standardGeneric("Introduction<-"))

#' @export
setMethod("Introduction", "SlickData", function(SlickData, lang=NULL) {
  get_language(SlickData@Introduction, lang)
  })

#' @export
setMethod("Introduction<-", "SlickData", function(SlickData, value) {
  if (is.null(value)) return(SlickData)
  SlickData@Introduction <- value
  SlickData
})

setGeneric("Date", function(SlickData) standardGeneric("Date"))
setGeneric("Date<-", function(SlickData, value) standardGeneric("Date<-"))

#' @export
setMethod("Date", "SlickData", function(SlickData) {SlickData@Date})

#' @export
setMethod("Date<-", "SlickData", function(SlickData, value) {
  if (is.null(value)) return(SlickData)
  if (inherits(value, 'POSIXct'))
    value <- value |> as.Date() |> as.character()
  SlickData@Date <- value
  SlickData
})


setGeneric("Author", function(SlickData) standardGeneric("Author"))
setGeneric("Author<-", function(SlickData, value) standardGeneric("Author<-"))

#' @export
setMethod("Author", "SlickData", function(SlickData) {
  SlickData@Author
})

#' @export
setMethod("Author<-", "SlickData", function(SlickData, value) {
  if (is.null(value)) return(SlickData)
  SlickData@Author <- value
  SlickData
})


setGeneric("Email", function(SlickData) standardGeneric("Email"))
setGeneric("Email<-", function(SlickData, value) standardGeneric("Email<-"))

#' @export
setMethod("Email", "SlickData", function(SlickData) {
  SlickData@Email
})

#' @export
setMethod("Email<-", "SlickData", function(SlickData, value) {
  if (is.null(value)) return(SlickData)
  SlickData@Email <- value
  SlickData
})

setGeneric("Institution", function(SlickData) standardGeneric("Institution"))
setGeneric("Institution<-", function(SlickData, value) standardGeneric("Institution<-"))

#' @export
setMethod("Institution", "SlickData", function(SlickData) {
  SlickData@Institution
})

#' @export
setMethod("Institution<-", "SlickData", function(SlickData, value) {
  if (is.null(value)) return(SlickData)
  SlickData@Institution <- value
  SlickData
})





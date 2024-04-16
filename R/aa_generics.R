
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


# ---- setGenerics -----

setGeneric("Author", function(x) standardGeneric("Author"))
setGeneric("Author<-", function(x, value) standardGeneric("Author<-"))

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

setGeneric("MaxColor", function(x) standardGeneric("MaxColor"))
setGeneric("MaxColor<-", function(x, value) standardGeneric("MaxColor<-"))

setGeneric("MinColor", function(x) standardGeneric("MinColor"))
setGeneric("MinColor<-", function(x, value) standardGeneric("MinColor<-"))

setGeneric("Metadata", function(x, ...) standardGeneric("Metadata"))
setGeneric("Metadata<-", function(x, value) standardGeneric("Metadata<-"))

setGeneric("MPs", function(x, ...) standardGeneric("MPs"))
setGeneric("MPs<-", function(x, value) standardGeneric("MPs<-"))

setGeneric("OMs", function(x, ...) standardGeneric("OMs"))
setGeneric("OMs<-", function(x, value) standardGeneric("OMs<-"))

setGeneric("Preset", function(x, ...) standardGeneric("Preset"))
setGeneric("Preset<-", function(x, value) standardGeneric("Preset<-"))

setGeneric("Quilt", function(x, ...) standardGeneric("Quilt"))
setGeneric("Quilt<-", function(x, value) standardGeneric("Quilt<-"))

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

setGeneric("X", function(x) standardGeneric("X"))
setGeneric("X<-", function(x, value) standardGeneric("X<-"))

setGeneric("Y", function(x) standardGeneric("Y"))
setGeneric("Y<-", function(x, value) standardGeneric("Y<-"))

# ---- setMethods -----



#' @export
setMethod("Metadata<-", "Boxplot", function(x, value) {
  x@Metadata <- value
  validObject(x)
  x
})

#' @export
setMethod("Table", "Boxplot", function(x, lang=NULL) {
  tableBoxplot(x, lang)
})

## MPs ----

#' @export
setMethod("MPs","dataframe_list",function(x, ...) newMPs(x, ...))

#' @export
setMethod("MPs","missingOrNULL",function(x, ...) newMPs(...))

#' @export
setMethod("Metadata", "MPs", function(x, lang=NULL) {
  get_language(x@Metadata, lang)
})

#' @export
setMethod("Metadata<-", "MPs", function(x, value) {
  x@Metadata <- value
  validObject(x)
  x
})

#' @export
setMethod("Preset", "MPs", function(x, lang=NULL) {
  get_language(x@Preset, lang)
})

#' @export
setMethod("Preset<-", "MPs", function(x, value) {
  x@Preset <- value
  validObject(x)
  x
})


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
  validObject(x)
  x
})

#' create a new OMs object
#' @export
setMethod("OMs","missingOrNULL",function(x, ...) newOMs(...))

#' @rdname OMs
#' @export
setMethod("OMs","dataframe_list",function(x, ...) newOMs(x, ...))

#' @rdname OMs
#' @export
setMethod("Metadata", "OMs", function(x, lang=NULL) {
  get_language(x@Metadata, lang)
})

#' @rdname OMs
#' @export
setMethod("Metadata<-", "OMs", function(x, value) {
  x@Metadata <- value
  validObject(x)
  x
})


#' @rdname OMs
#' @export
setMethod("Preset", "OMs", function(x, ...) {
  x@Preset
})

#' @rdname OMs
#' @export
setMethod("Preset<-", "OMs", function(x, value) {
  x@Preset <- value
  validObject(x)
  x
})

#' @export
setMethod("Table", "OMs", function(x, lang=NULL, type=NULL) {
  tableOMs(x, lang, type)
})



## Boxplot ----

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


## Kobe ----

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
  validObject(x)
  x
})

#' @export
setMethod("Time", "Kobe", function(x) {x@Time})

#' @export
setMethod("Time<-", "Kobe", function(x, value) {
  x@Time <- value
  validObject(x)
  x
})


#' @export
setMethod("TimeLab", "Kobe", function(x) {x@TimeLab})

#' @export
setMethod("TimeLab<-", "Kobe", function(x, value) {
  x@TimeLab <- value
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



## Quilt ----

#' @export
setMethod("MaxColor", "Quilt", function(x) {x@MaxColor})

#' @export
setMethod("MaxColor<-", "Quilt", function(x, value) {
  x@MaxColor <- value
  validObject(x)
  x
})

#' @export
setMethod("MinColor", "Quilt", function(x) {x@MinColor})

#' @export
setMethod("MinColor<-", "Quilt", function(x, value) {
  x@MinColor <- value
  validObject(x)
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
setMethod("Preset", "Quilt", function(x, ...) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Quilt", function(x, value) {
  x@Preset <- value
  validObject(x)
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
  validObject(x)
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
  validObject(x)
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
  validObject(x)
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
  validObject(x)
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
  validObject(x)
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
  validObject(x)
  x
})


#' @export
setMethod("MPs", "Slick", function(x, lang=NULL) {
  get_language(x@MPs, lang)
} )

#' @export
setMethod("MPs<-", "Slick", function(x, value) {
  x@MPs <- value
  validObject(x)
  x
})

#' assign an OMs object
#' @export
setMethod("OMs<-", "Slick", function(x, value) {
  x@OMs <- value
  validObject(x)
  x
})

#' print an OMs object
#' @export
setMethod("OMs", "Slick", function(x) {
  x@OMs
})

#' @export
setMethod("Spider<-", "Slick", function(x, value) {
  x@Spider <- value
  x
})

#' @export
setMethod("Spider","Slick",function(x, ...) x@Spider)

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
  validObject(x)
  x
})

#' @export
setMethod("show", "Slick", function(object) {
  cat('An object of class `Slick` \n\n')
  cat('Title:', Title(object, 'en'), '\n')
  cat('Subtitle:', Subtitle(object, 'en'), '\n')
  cat('Author:', paste(Author(object),collapse=', '), '\n')

})

#' @export
setMethod("Title", "Slick", function(x, lang=NULL) {
  get_language(x@Title, lang)
})

#' @export
setMethod("Title<-", "Slick", function(x, value) {
  x@Title <- value
  validObject(x)
  x
})

#' @export
setMethod("Timeseries","Slick",function(x, ...) x@Timeseries)

#' @export
setMethod("Timeseries", "Slick", function(x) {
  x@Timeseries
})

#' @export
setMethod("Timeseries<-", "Slick", function(x, value) {
  x@Timeseries <- value
  x
})

#' @export
setMethod("Tradeoff","Slick",function(x, ...) x@Tradeoff)

#' @export
setMethod("Tradeoff", "Slick", function(x) {
  x@Tradeoff
})

#' @export
setMethod("Tradeoff<-", "Slick", function(x, value) {
  x@Tradeoff <- value
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
setMethod("Metadata", "Spider", function(x, lang=NULL) {
  get_language(x@Metadata, lang)

})

#' @export
setMethod("Metadata<-", "Spider", function(x, value) {
  x@Metadata <- value
  x
})

#' @export
setMethod("Preset", "Spider", function(x, ...) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Spider", function(x, value) {
  x@Preset <- value
  validObject(x)
  x
})

#' @export
setMethod("Table", "Spider", function(x, lang=NULL) {
  tableSpider(x, lang)
})


## Timeseries ----

#' @export
setMethod("Timeseries","character_list",function(x, ...) newTimeseries(x, ...))

#' @export
setMethod("Timeseries","missingOrNULL",function(x, ...) newTimeseries(...))

#' @export
setMethod("Timeseries","character",function(x, ...) newTimeseries(x, ...))



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
setMethod("Value", "Tradeoff", function(x) {
  x@Value
})

#' @export
setMethod("X", "Tradeoff", function(x) {
  x@X
})

#' @export
setMethod("X<-", "Tradeoff", function(x, value=NULL) {
  x@X <- value
  validObject(x)
  x
})

#' @export
setMethod("Y", "Tradeoff", function(x) {
  x@Y
})

#' @export
setMethod("Y<-", "Tradeoff", function(x, value=NULL) {
  x@Y <- value
  validObject(x)
  x
})

#' @export
setMethod("Preset", "Tradeoff", function(x) {
  x@Preset
})

#' @export
setMethod("Preset<-", "Tradeoff", function(x, value) {
  x@Preset <- value
  validObject(x)
  x
})


#### NEED TO UPDATE #####





## Design ----

#' @rdname OMs
#' @export
setMethod("Design", "OMs", function(x) {
  x@Design
})

#' @rdname OMs
#' @export
setMethod("Design<-", "OMs", function(x, value) {
  x@Design <- value
  x
})





## show ----


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







## Design ----



#' @export
setMethod("Design", "Slick", function(x) {
  x@OMs@Design
})

#' @export
setMethod("Design<-", "Slick", function(x, value) {
  x@OMs@Design <- value
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
setMethod("Boxplot","Slick",function(x, ...) x@Boxplot)

#' @export
setMethod("Boxplot", "Slick", function(x) {
  x@Boxplot
})

#' @export
setMethod("Boxplot<-", "Slick", function(x, value) {
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
setMethod("Kobe","Slick",function(x, ...) x@Kobe)

#' @export
setMethod("Kobe", "Slick", function(x) {
  x@Kobe
})

#' @export
setMethod("Kobe<-", "Slick", function(x, value) {
  x@Kobe <- value
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
setMethod("Quilt","Slick",function(x, ...) x@Quilt)

#' @export
setMethod("Quilt<-", "Slick", function(x, value) {
  x@Quilt <- value
  x
})

## Min and Max ----
setGeneric("Min", function(x) standardGeneric("Min"))
setGeneric("Min<-", function(x, value) standardGeneric("Min<-"))
setGeneric("Max", function(x) standardGeneric("Max"))
setGeneric("Max<-", function(x, value) standardGeneric("Max<-"))

#' @export
setMethod("Min","Quilt",function(x) Quilt@Min)

#' @export
setMethod("Min<-","Quilt",function(x, value) {
  Quilt@Min <- value
})

#' @export
setMethod("Max","Quilt",function(x) Quilt@Max)

#' @export
setMethod("Max<-","Quilt",function(x, value) {
  Quilt@Max <- value
})



































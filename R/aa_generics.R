
#' @include class_Boxplot.R
#' @include class_Kobe.R
#' @include class_OMs.R
#' @include class_Quilt.R
#' @include class_Slick.R
#' @include class_Spider.R
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

setGeneric("Quilt", function(x, ...) standardGeneric("Quilt"))
setGeneric("Quilt<-", function(x, value) standardGeneric("Quilt<-"))

setGeneric("Spider", function(x, ...) standardGeneric("Spider"))
setGeneric("Spider<-", function(x, value) standardGeneric("Spider<-"))

setGeneric("Subtitle", function(x, ...) standardGeneric("Subtitle"))
setGeneric("Subtitle<-", function(x, value) standardGeneric("Subtitle<-"))

setGeneric("Timeseries", function(x, ...) standardGeneric("Timeseries"))
setGeneric("Timeseries<-", function(x, value) standardGeneric("Timeseries<-"))

setGeneric("Title", function(x, ...) standardGeneric("Title"))
setGeneric("Title<-", function(x, value) standardGeneric("Title<-"))

setGeneric("Value", function(x, ...) standardGeneric("Value"))
setGeneric("Value<-", function(x, value) standardGeneric("Value<-"))


# ---- setMethods -----



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
setMethod("Metadata", "OMs", function(x, ...) {
  get_language(x@Metadata, lang)
})

#' @rdname OMs
#' @export
setMethod("Metadata<-", "OMs", function(x, value) {
  x@Metadata <- value
  validObject(x)
  x
})




## Quilt ----

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
setMethod("Value", "Quilt", function(x) {x@Value})

#' @export
setMethod("Value<-", "Quilt", function(x, value) {
  x@Value <- value
  validObject(x)
  x
})

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

## Spider ----

#' @export
setMethod("Spider","character_list",function(x, ...) newSpider(x, ...))

#' @export
setMethod("Spider","missingOrNULL",function(x, ...) newSpider(...))

#' @export
setMethod("Spider","character",function(x, ...) newSpider(x, ...))

#' @export
setMethod("Spider","Slick",function(x, ...) x@Spider)

#' @export
setMethod("Spider", "Slick", function(x) {
  x@Spider
})

#' @export
setMethod("Spider<-", "Slick", function(x, value) {
  x@Spider <- value
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

## Default ----

setGeneric("Default", function(x, ...) standardGeneric("Default"))
setGeneric("Default<-", function(x, value) standardGeneric("Default<-"))

#' @export
setMethod("Default", "OMs", function(x) {x@Default})

#' @export
setMethod("Default<-", "OMs", function(x, value) {
  if (!inherits(value, 'list')) {
    stop('`Default` must be class `list`', call.=FALSE)
  }
  x@Default <- value
  validObject(x)
  x
})




#' @export
setMethod("Default", "Quilt", function(x) {x@Default})

#' @export
setMethod("Default<-", "Quilt", function(x, value) {
  if (!inherits(value, 'numeric')) {
    stop('`Default` must be class `numeric`', call.=FALSE)
  }
  x@Default <- value
  validObject(x)
  x
})


#' @export
setMethod("Default", "Boxplot", function(x) {x@Default})

#' @export
setMethod("Default<-", "Boxplot", function(x, value) {
  if (!inherits(value, 'numeric')) {
    stop('`Default` must be class `numeric`', call.=FALSE)
  }
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



































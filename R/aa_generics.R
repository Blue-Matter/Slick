


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
setGeneric("OMs", function(x, ...) standardGeneric("OMs"))
setGeneric("OMs<-", function(x, value) standardGeneric("OMs<-"))

#' @export
setMethod("OMs","data.frame",function(x, ...) newOMs(x, ...))

#' @export
setMethod("OMs","missingOrNULL",function(x, ...) newOMs(...))

#' @export
setMethod("OMs", "SlickData", function(x) {
  x@OMs
})

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


# Description  -----
setGeneric("Description", function(x, ...) standardGeneric("Description"))
setGeneric("Description<-", function(x, value) standardGeneric("Description<-"))


#' @export
setMethod("Description", "OMs", function(x) {x@Description})

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
setMethod("Description", "MPs", function(x) {x@Description})

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
setMethod("Description", "Quilt", function(x) {x@Description})

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
setMethod("Description", "Spider", function(x) {x@Description})


#' @export
setMethod("Description<-", "Spider", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Description` must be class `character`', call.=FALSE)
  }
  x@Description <- value
  validObject(x)
  x
})

#' @describeIn newBoxplot Returns Description of an object of class `Boxplot`
#' @export
setMethod("Description", "Boxplot", function(x) {x@Description})

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
setMethod("Description", "Kobe", function(x) {x@Description})

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

#' @describeIn newTimeseries Returns Description of an object of class `Timeseries`
#' @export
setMethod("Description", "Timeseries", function(x) {x@Description})

#' @describeIn newTimeseries Assigns Description for an object of class `Timeseries`
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
setGeneric("Label", function(x, ...) standardGeneric("Label"))
setGeneric("Label<-", function(x, value) standardGeneric("Label<-"))


#' @export
setMethod("Label", "OMs", function(x) {x@Label})

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
setMethod("Label", "MPs", function(x) {x@Label})

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
setMethod("Label", "Quilt", function(x) {x@Label})

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
setMethod("Label", "Spider", function(x) {x@Label})

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
setMethod("Label", "Boxplot", function(x) {x@Label})

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
setMethod("Label", "Kobe", function(x) {x@Label})

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
setMethod("Label", "Timeseries", function(x) {x@Label})

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
  if (!inherits(value, 'character')) {
    stop('`Value` must be class `character`', call.=FALSE)
  }
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Spider", function(x) {x@Value})

#' @export
setMethod("Value<-", "Spider", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Value` must be class `character`', call.=FALSE)
  }
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Boxplot", function(x) {x@Value})

#' @export
setMethod("Value<-", "Boxplot", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Value` must be class `character`', call.=FALSE)
  }
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Kobe", function(x) {x@Value})

#' @export
setMethod("Value<-", "Kobe", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Value` must be class `character`', call.=FALSE)
  }
  x@Value <- value
  validObject(x)
  x
})

#' @export
setMethod("Value", "Timeseries", function(x) {x@Value})

#' @export
setMethod("Value<-", "Timeseries", function(x, value) {
  if (!inherits(value, 'character')) {
    stop('`Value` must be class `character`', call.=FALSE)
  }
  x@Value <- value
  validObject(x)
  x
})

## Default ---

setGeneric("Default", function(x, ...) standardGeneric("Default"))
setGeneric("Default<-", function(x, value) standardGeneric("Default<-"))

#' @export
setMethod("Default", "OMs", function(x) {x@Label})

#' @export
setMethod("Default<-", "OMs", function(x, value) {
  if (!inherits(value, 'list')) {
    stop('`Default` must be class `list`', call.=FALSE)
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

setGeneric("Title", function(x, ...) standardGeneric("Title"))
setGeneric("Title<-", function(x, value) standardGeneric("Title<-"))

#' @export
setMethod("Title", "SlickData", function(x) {x@Title})
#' @export
setMethod("Title<-", "SlickData", function(x, value) {
  x@Title <- value
  x
})


setGeneric("Subtitle", function(x, ...) standardGeneric("Subtitle"))
setGeneric("Subtitle<-", function(x, value) standardGeneric("Subtitle<-"))

#' @export
setMethod("Subtitle", "SlickData", function(x, value) {x@Subtitle})

#' @export
setMethod("Subtitle<-", "SlickData", function(x, value) {
  x@Subtitle <- value
  x
})


setGeneric("Fishery", function(x) standardGeneric("Fishery"))
setGeneric("Fishery<-", function(x, value) standardGeneric("Fishery<-"))

#' @export
setMethod("Fishery", "SlickData", function(x) {x@Fishery})

#' @export
setMethod("Fishery<-", "SlickData", function(x, value) {
  x@Fishery <- value
  x
})

setGeneric("Introduction", function(x) standardGeneric("Introduction"))
setGeneric("Introduction<-", function(x, value) standardGeneric("Introduction<-"))

#' @export
setMethod("Introduction", "SlickData", function(x) {x@Introduction})

#' @export
setMethod("Introduction<-", "SlickData", function(x, value) {
  x@Introduction <- value
  x
})

setGeneric("Date", function(x) standardGeneric("Date"))
setGeneric("Date<-", function(x, value) standardGeneric("Date<-"))

#' @export
setMethod("Date", "SlickData", function(x) {x@Date})

#' @export
setMethod("Date<-", "SlickData", function(x, value) {
  x@Date <- value
  x
})


setGeneric("Author", function(x) standardGeneric("Author"))
setGeneric("Author<-", function(x, value) standardGeneric("Author<-"))

#' @export
setMethod("Author", "SlickData", function(x) {
  x@Author
})

#' @export
setMethod("Author<-", "SlickData", function(x, value) {
  x@Author <- value
  x
})


setGeneric("Email", function(x) standardGeneric("Email"))
setGeneric("Email<-", function(x, value) standardGeneric("Email<-"))

#' @export
setMethod("Email", "SlickData", function(x) {
  x@Email
})

#' @export
setMethod("Email<-", "SlickData", function(x, value) {
  x@Email <- value
  x
})

setGeneric("Institution", function(x) standardGeneric("Institution"))
setGeneric("Institution<-", function(x, value) standardGeneric("Institution<-"))

#' @export
setMethod("Institution", "SlickData", function(x) {
  x@Institution
})

#' @export
setMethod("Institution<-", "SlickData", function(x, value) {
  x@Institution <- value
  x
})





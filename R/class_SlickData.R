
split_by_semicolon <- function(string) {
  if (grepl(';',string)) {
    string <- strsplit(string, ';')[[1]]
    string <- gsub("\\.", "", string)
    string <- gsub('[[:digit:]]+', '', string)
    return(trimws(string))
  }
  trimws(string)
}


na_if_empty <- function(val) {
  if (length(val)<1) return(NA)
  val
}

check_slot_class <- function(object, slot, value) {
  cl_obj <- class(slot(object, slot))
  cl_val <- class(value)
  if (!cl_val %in% cl_obj)
    stop('`', slot, '` must be class `', cl_obj, '`', call.=FALSE)
  value
}

use_ifnot_NULL <- function(slot, value, object) {
  if (!is.null(value)) {
    slot(object, slot) <- value
  }
  slot(object, slot)
}


# ---- Class OMs ----

setClass("OMs",
         slots=c(Design='data.frame',
                 Description='list',
                 Labels='list'
                )
)

setMethod("initialize", "OMs", function(.Object,
                                        Design=NULL,
                                        Description=NULL,
                                        Labels=NULL) {

  if (!is.null(Design)) {
    .Object@Design <- Design
  }

  if (!is.null(Description)) {
    .Object@Description <- Description
  }

  if (!is.null(Labels)) {
    .Object@Labels <- Labels
  }
  .Object
})

validOMs <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('OMs', validOMs)

NewOMs <- function(Design=NULL,
                Description=NULL,
                Labels=NULL) {
  OMs <- new('OMs', Design, Description, Labels)
  validObject(OMs)
  OMs
}

# ---- Class MPs ----

setClass("MPs",
         slots=c(Labels='character',
                 Description='character',
                 Link='character'
         )
)


setMethod("initialize", "MPs", function(.Object,
                                        Labels=NULL,
                                        Description=NULL,
                                        Link=NULL) {
  .Object@Labels <- use_ifnot_NULL('Labels', Labels, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Link <- use_ifnot_NULL('Link', Link, .Object)
  .Object
})

validMPs <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('MPs', validMPs)

NewMPs <- function(Labels=NULL,
                Description=NULL,
                Link=NULL) {
  MPs <- new('MPs', Labels, Description, Link)
  validObject(MPs)
  MPs
}


# ---- Class Quilt ----

setClass("Quilt",
         slots=c(Name='character',
                 Description='character',
                 Value='list'
         )
)

setMethod("initialize", "Quilt", function(.Object,
                                          Name=NULL,
                                          Description=NULL,
                                          Value=NULL) {
  .Object@Name <- use_ifnot_NULL('Name', Name, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object
})


validQuilt <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Quilt', validQuilt)

NewQuilt <- function(Name=NULL,
                  Description=NULL,
                  Value=NULL) {
  Quilt <- new('Quilt', Name, Description, Value)
  validObject(Quilt)
  Quilt
}



# ---- Class Spider ----

setClass("Spider",
         slots=c(Name='character',
                 Description='character',
                 Value='list'
         )
)

setMethod("initialize", "Spider", function(.Object,
                                          Name=NULL,
                                          Description=NULL,
                                          Value=NULL) {
  .Object@Name <- use_ifnot_NULL('Name', Name, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object
})


validSpider <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Spider', validSpider)

NewSpider <- function(Name=NULL,
                  Description=NULL,
                  Value=NULL) {
  Spider <- new('Spider', Name, Description, Value)
  validObject(Spider)
  Spider
}


# ---- Class Boxplot ----

setClass("Boxplot",
         slots=c(Name='character',
                 Description='character',
                 Value='list'
         )
)

setMethod("initialize", "Boxplot", function(.Object,
                                           Name=NULL,
                                           Description=NULL,
                                           Value=NULL) {
  .Object@Name <- use_ifnot_NULL('Name', Name, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object
})


validBoxplot <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Boxplot', validBoxplot)

NewBoxplot <- function(Name=NULL,
                   Description=NULL,
                   Value=NULL) {
  Boxplot <- new('Boxplot', Name, Description, Value)
  validObject(Boxplot)
  Boxplot
}

# ---- Class Kobe ----

setClass("Kobe",
         slots=c(Name='character',
                 Description='character',
                 Time='numeric',
                 TimeLab='character',
                 Value='list',
                 RefPoints='list',
                 RefNames='list'
         )
)

setMethod("initialize", "Kobe", function(.Object,
                                         Name=NULL,
                                         Description=NULL,
                                         Time=NULL,
                                         TimeLab=NULL,
                                         Value=NULL,
                                         RefPoints=NULL,
                                         RefNames=NULL) {
  .Object@Name <- use_ifnot_NULL('Name', Name, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@TimeLab <- use_ifnot_NULL('TimeLab', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@RefPoints <- use_ifnot_NULL('RefPoints', RefPoints, .Object)
  .Object@RefNames <- use_ifnot_NULL('RefNames', RefNames, .Object)
  .Object
})


validKobe <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('Kobe', validKobe)

NewKobe <- function(Name=NULL,
                 Description=NULL,
                 Time=NULL,
                 TimeLab=NULL,
                 Value=NULL,
                 RefPoints=NULL,
                 RefNames=NULL) {
  Kobe <- new('Kobe', Name, Description, Value,
              Time, TimeLab, Value, RefPoints, RefNames)
  validObject(Kobe)
  Kobe
}


# ---- Class TimeSeries ----


setClass("TimeSeries",
         slots=c(Name='character',
                 Description='character',
                 Time='numeric',
                 TimeNow='numeric',
                 TimeLab='character',
                 Value='list',
                 RefPoints='list',
                 RefNames='list'
         )
)

setMethod("initialize", "TimeSeries", function(.Object,
                                         Name=NULL,
                                         Description=NULL,
                                         Time=NULL,
                                         TimeNow=NULL,
                                         TimeLab=NULL,
                                         Value=NULL,
                                         RefPoints=NULL,
                                         RefNames=NULL) {
  .Object@Name <- use_ifnot_NULL('Name', Name, .Object)
  .Object@Description <- use_ifnot_NULL('Description', Description, .Object)
  .Object@Time <- use_ifnot_NULL('Time', Time, .Object)
  .Object@TimeNow <- use_ifnot_NULL('TimeNow', TimeNow, .Object)
  .Object@TimeLab <- use_ifnot_NULL('TimeLab', Description, .Object)
  .Object@Value <- use_ifnot_NULL('Value', Value, .Object)
  .Object@RefPoints <- use_ifnot_NULL('RefPoints', RefPoints, .Object)
  .Object@RefNames <- use_ifnot_NULL('RefNames', RefNames, .Object)
  .Object
})

validTimeSeries <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('TimeSeries', validTimeSeries)

NewTimeSeries <- function(Name=NULL,
                 Description=NULL,
                 Time=NULL,
                 TimeLab=NULL,
                 Value=NULL,
                 RefPoints=NULL,
                 RefNames=NULL) {
  TimeSeries <- new('TimeSeries', Name, Description, Value,
              Time, TimeLab, TimeNow, Value, RefPoints, RefNames)
  validObject(TimeSeries)
  TimeSeries
}


# ---- SlickData Class ----

#' Class \code{'SlickData'}
#'
#' A `Slick` Data object for uploading into the Slick App.
#'
#'
#' @author A. Hordyk
#' @keywords classes
#' @docType class
#' @export
#'
setClass("SlickData",
         slots=c(Title='character',
                 Subtitle='character',
                 Fishery='character',
                 Introduction='list',
                 Date='character',
                 Author='character',
                 Email='character',
                 Institution='character',
                 OMs='OMs',
                 MPs='MPs',
                 Quilt='Quilt',
                 Spider='Spider',
                 Boxplot='Boxplot',
                 Kobe='Kobe',
                 TimeSeries='TimeSeries')
)

setGeneric("Title", function(x, ...) standardGeneric("Title"))
setGeneric("Title<-", function(x, value) standardGeneric("Title<-"))
setMethod("Title", "SlickData", function(x) {x@Title})
setMethod("Title<-", "SlickData", function(x, value) {
  x@Title <- value
  x
})

setGeneric("Subtitle", function(x, ...) standardGeneric("Subtitle"))
setGeneric("Subtitle<-", function(x, value) standardGeneric("Subtitle<-"))
setMethod("Subtitle", "SlickData", function(x, value) {x@Subtitle})
setMethod("Subtitle<-", "SlickData", function(x, value) {
  x@Subtitle <- value
  x
})

setGeneric("Fishery", function(x) standardGeneric("Fishery"))
setGeneric("Fishery<-", function(x, value) standardGeneric("Fishery<-"))
setMethod("Fishery", "SlickData", function(x) {x@Fishery})
setMethod("Fishery<-", "SlickData", function(x, value) {
  x@Fishery <- value
  x
})

setGeneric("Introduction", function(x) standardGeneric("Introduction"))
setGeneric("Introduction<-", function(x, value) standardGeneric("Introduction<-"))
setMethod("Introduction", "SlickData", function(x) {x@Introduction})
setMethod("Introduction<-", "SlickData", function(x, value) {
  x@Introduction <- value
  x
})

setGeneric("Date", function(x) standardGeneric("Date"))
setGeneric("Date<-", function(x, value) standardGeneric("Date<-"))
setMethod("Date", "SlickData", function(x) {x@Date})
setMethod("Date<-", "SlickData", function(x, value) {
  x@Date <- value
  x
})


setGeneric("Author", function(x) standardGeneric("Author"))
setGeneric("Author<-", function(x, value) standardGeneric("Author<-"))

setMethod("Author", "SlickData", function(x) {
  x@Author
})
setMethod("Author<-", "SlickData", function(x, value) {
  x@Author <- value
  x
})


setGeneric("Email", function(x) standardGeneric("Email"))
setGeneric("Email<-", function(x, value) standardGeneric("Email<-"))

setMethod("Email", "SlickData", function(x) {
  x@Email
})
setMethod("Email<-", "SlickData", function(x, value) {
  x@Email <- value
  x
})

setGeneric("Institution", function(x) standardGeneric("Institution"))
setGeneric("Institution<-", function(x, value) standardGeneric("Institution<-"))

setMethod("Institution", "SlickData", function(x) {
  x@Institution
})
setMethod("Institution<-", "SlickData", function(x, value) {
  x@Institution <- value
  x
})

setGeneric("OMs", function(x) standardGeneric("OMs"))
setGeneric("OMs<-", function(x, value) standardGeneric("OMs<-"))

setMethod("OMs", "SlickData", function(x) {
  x@OMs
})
setMethod("OMs<-", "SlickData", function(x, value) {
  x@OMs <- value
  x
})

setGeneric("MPs", function(x) standardGeneric("MPs"))
setGeneric("MPs<-", function(x, value) standardGeneric("MPs<-"))

setMethod("MPs", "SlickData", function(x) {
  x@MPs
})
setMethod("MPs<-", "SlickData", function(x, value) {
  x@MPs <- value
  x
})


setMethod("initialize", "SlickData", function(.Object,
                                              Title=NULL,
                                              Subtitle=NULL,
                                              Fishery=NULL,
                                              Introduction=NULL,
                                              Date=NULL,
                                              Author=NULL,
                                              Email=NULL,
                                              Institution=NULL,
                                              OMs=NULL,
                                              MPs=NULL,
                                              Quilt=NULL,
                                              Spider=NULL,
                                              Boxplot=NULL,
                                              Kobe=NULL,
                                              TimeSeries=NULL) {

  .Object@Title <- use_ifnot_NULL('Title', Title, .Object)
  .Object@Subtitle <- use_ifnot_NULL('Subtitle', Subtitle, .Object)
  .Object@Fishery <- use_ifnot_NULL('Fishery', Fishery, .Object)
  .Object@Introduction <- use_ifnot_NULL('Introduction', Introduction, .Object)
  .Object@Date <- use_ifnot_NULL('Date', Date, .Object)
  .Object@Author <- use_ifnot_NULL('Author', Author, .Object)
  .Object@Email <- use_ifnot_NULL('Email', Email, .Object)
  .Object@Institution <- use_ifnot_NULL('Institution', Institution, .Object)
  .Object@OMs <- use_ifnot_NULL('OMs', OMs, .Object)
  .Object@MPs <- use_ifnot_NULL('MPs', MPs, .Object)
  .Object@Quilt <- use_ifnot_NULL('Quilt', Quilt, .Object)
  .Object@Spider <- use_ifnot_NULL('Spider', Spider, .Object)
  .Object@Boxplot <- use_ifnot_NULL('Boxplot', Boxplot, .Object)
  .Object@Kobe <- use_ifnot_NULL('Kobe', Kobe, .Object)
  .Object@TimeSeries <- use_ifnot_NULL('TimeSeries', TimeSeries, .Object)
  .Object
})


validSlickData <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('SlickData', validSlickData)

NewSlick <- function(Title=NULL,
                      Subtitle=NULL,
                      Fishery=NULL,
                      Introduction=NULL,
                      Date=NULL,
                      Author=NULL,
                      Email=NULL,
                      Institution=NULL,
                      OMs=NULL,
                      MPs=NULL,
                      Quilt=NULL,
                      Spider=NULL,
                      Boxplot=NULL,
                      Kobe=NULL,
                      TimeSeries=NULL) {
  SlickData <- new('SlickData', Title,
                   Subtitle,
                   Fishery,
                   Introduction,
                   Date,
                   Author, Email, Institution,
                   OMs, MPs, Quilt,
                   Spider, Boxplot,Kobe, TimeSeries)
  validObject(SlickData)
  SlickData
}

slick <- readRDS('inst/shiny_apps/Slick/data/case_studies/WSKJ.slick')


setGeneric("Design", function(x) standardGeneric("Design"))
setGeneric("Design<-", function(x, value) standardGeneric("Design<-"))

setMethod("Design", "OMs", function(x) {
  x@Design
})
setMethod("Design<-", "OMs", function(x, value) {
  x@Design <- value
  x
})


setMethod("Design", "SlickData", function(x) {
  x@OMs@Design
})
setMethod("Design<-", "SlickData", function(x, value) {
  x@OMs@Design <- value
  x
})

setGeneric("Description", function(x) standardGeneric("Description"))
setGeneric("Description<-", function(x, value) standardGeneric("Description<-"))
setMethod("Description", "OMs", function(x) {
  x@Description
})
setMethod("Description<-", "OMs", function(x, value) {
  x@Description <- value
  x
})

setMethod("Description", "MPs", function(x) {
  x@Description
})
setMethod("Description<-", "MPs", function(x, value) {
  x@Description <- value
  x
})


setGeneric("Labels", function(x) standardGeneric("Labels"))
setGeneric("Labels<-", function(x, value) standardGeneric("Labels<-"))
setMethod("Labels", "OMs", function(x) {
  x@Labels
})
setMethod("Labels<-", "OMs", function(x, value) {
  x@Labels <- value
  x
})

setMethod("Labels", "MPs", function(x) {
  x@Labels
})
setMethod("Labels<-", "MPs", function(x, value) {
  x@Labels <- value
  x
})



slick2SlickData <- function(slick) {

  out <- NewSlick()
  Title(out) <- slick$Text$Title
  Subtitle(out) <- slick$Text$Sub_title
  Fishery(out) <- slick$Misc$Fishery
  Introduction(out) <- slick$Text$Introduction
  Date(out) <- slick$Misc$Date
  Author(out) <- split_by_semicolon(slick$Misc$Author)
  Email(out) <- split_by_semicolon(slick$Misc$Contact)
  Institution(out) <- split_by_semicolon(slick$Misc$Institution)

  # OMs
  oms <- NewOMs()
  Design(oms) <- data.frame(slick$OM$Design)
  colnames(Design(oms)) <- slick$OM$Factor_Labels

  for (i in 1:ncol(Design(oms))) {
    Design(oms)[,i] <- slick$OM$Codes[[i]][Design(oms)[,i]]
  }

  Description(oms) <-  slick$OM$Description
  Labels(oms) <- slick$OM$Labels
  OMs(out) <- oms

  # MPs
  mps <- NewMPs()
  Labels(mps) <- slick$MP$Labels
  Description(mps) <- slick$MP$Description
  MPs(out) <- mps

  # Quilt

  # Spider

  # Boxplot

  # Kobe

  # TimeSeries


  out
}



obj <- readRDS('inst/shiny_apps/Slick/data/case_studies/WSKJ.slick')

# obj$Perf$Det$Labels
obj$Perf$Det$Codes
obj$Perf$Det$Description

obj$MP$Labels |> length()
obj$Perf$Det$Labels |> length()
obj$Perf$Det$Values |> dim()

obj$Perf$Det$RefNames
obj$Perf$Det$RefPoints

obj$OM$Factor_Labels

obj$OM$Codes


# es
# fr




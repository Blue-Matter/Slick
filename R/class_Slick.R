


# ---- Class ----

#' S4 class `Slick` and associated functions
#'
#' The `Slick` class is the main object class used in the `Slick` package. It
#' contains sub-objects for the management procedures [MPs()], operating models [OMs()],
#' and the six chart types: [Boxplot()], [Kobe()], [Quilt()], [Spider()],
#' [Timeseries()], and [Tradeoff()], as well as metadata information for the `Slick`
#' object such as `Title`, `Author`, and `Introduction`.
#'
#' Objects of class `Slick` are created with `Slick()`.
#'
#' Like all S4 objects in `Slick`, slots in this object can be accessed and
#' assigned using functions corresponding to slot name. See `Usage` and `Functions` section.
#'
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' All functions with the exception of `Date` support Markdown.
#'
#' @slot Title `r title_param()`
#' @slot Subtitle `r subtitle_param()`
#' @slot Date `r date_param()`
#' @slot Author `r author_param()`
#' @slot Email `r email_param()`
#' @slot Institution `r institution_param()`
#' @slot Introduction `r introduction_param()`
#' @slot MPs An object of class [MPs-class()]
#' @slot OMs An object of class [OMs-class()]
#' @slot Boxplot An object of class [Boxplot-class()]
#' @slot Kobe An object of class [Kobe-class()]
#' @slot Quilt An object of class [Quilt-class()]
#' @slot Spider An object of class [Spider-class()]
#' @slot Timeseries An object of class [Timeseries-class()]
#' @slot Tradeoff An object of class [Tradeoff-class()]
#'
#' @seealso [MPs()], [OMs()], [Boxplot()], [Kobe()], [Quilt()],
#' [Spider()], [Timeseries()], [Tradeoff()], and [Check()]
#'
#' @include class_MPs.R
#' @include class_OMs.R
#' @include class_Boxplot.R
#' @include class_Quilt.R
#' @include class_Spider.R
#' @include class_Kobe.R
#' @include class_Timeseries.R
#' @include class_Tradeoff.R
#'
#' @example inst/examples/Slick.R
#'
#' @docType class
#' @export
#'
Slick <- setClass("Slick",
         slots=c(Title='character_list',
                 Subtitle='character_list',
                 Date='date_character',
                 Author='character',
                 Email='character',
                 Institution='character',
                 Introduction='character_list',
                 MPs='MPs',
                 OMs='OMs',
                 Boxplot='Boxplot',
                 Kobe='Kobe',
                 Quilt='Quilt',
                 Spider='Spider',
                 Timeseries='Timeseries',
                 Tradeoff='Tradeoff')
)



## ---- Initialize ----
setMethod("initialize", "Slick", function(.Object,
                                          Title=NULL,
                                          Subtitle=NULL,
                                          Date=Sys.Date(),
                                          Author=NULL,
                                          Email=NULL,
                                          Institution=NULL,
                                          Introduction=NULL,
                                          MPs=NULL,
                                          OMs=new('OMs'),
                                          Boxplot=new('Boxplot'),
                                          Kobe=new('Kobe'),
                                          Quilt=new('Quilt'),
                                          Spider=new('Spider'),
                                          Timeseries=new('Timeseries'),
                                          Tradeoff=new('Tradeoff')) {

  .Object@Title <- use_ifnot_NULL('Title', Title, .Object)
  .Object@Subtitle <- use_ifnot_NULL('Subtitle', Subtitle, .Object)
  .Object@Date <- use_ifnot_NULL('Date', Date, .Object)
  .Object@Author <- use_ifnot_NULL('Author', Author, .Object)
  .Object@Email <- use_ifnot_NULL('Email', Email, .Object)
  .Object@Institution <- use_ifnot_NULL('Institution', Institution, .Object)
  .Object@Introduction <- use_ifnot_NULL('Introduction', Introduction, .Object)
  .Object@MPs <- use_ifnot_NULL('MPs', MPs, .Object)
  .Object@OMs <- use_ifnot_NULL('OMs', OMs, .Object)

  .Object@Boxplot <- use_ifnot_NULL('Boxplot', Boxplot, .Object)
  .Object@Kobe <- use_ifnot_NULL('Kobe', Kobe, .Object)
  .Object@Quilt <- use_ifnot_NULL('Quilt', Quilt, .Object)
  .Object@Spider <- use_ifnot_NULL('Spider', Spider, .Object)
  .Object@Timeseries <- use_ifnot_NULL('Timeseries', Timeseries, .Object)
  .Object@Tradeoff <- use_ifnot_NULL('Tradeoff', Tradeoff, .Object)
  .Object
})

## ---- Validate ----

check_data.frame <- function(obj, req, opt) {
  cnames <- colnames(obj)

  if (length(cnames)<1)
    return(NULL)

  if (!all(req %in% cnames))
    return(paste('Invalid dataframe. Must have columns:',
                 paste(req, collapse=', ')))

  if (!all(cnames %in% c(req, opt)))
    return(paste('Invalid dataframe. Can only have columns:',
                 paste(c(req, opt), collapse=', ')))

  NULL
}
validSlick <- function(object) {

  if (length(object@Title)>0) {
    if (inherits(object@Title, 'list')) {
      names(object@Title %in% c('en', 'es', 'fr'))
    }
  }

  # MPs
  obj <- MPs(object)
  if (inherits(obj, 'data.frame')) {
    test <- check_data.frame(obj,
                             req=c('Code', 'Label', 'Description'),
                             opt=c('Color', 'Default'))
    if (!is.null(test)) return(test)
  }
  if (inherits(obj, 'list')) {
    test <- lapply(obj, check_data.frame, req=c('Code', 'Label', 'Description'),
                   opt=c('Color', 'Default'))
    if (!all(unlist(lapply(test, is.null)))) return(test)
  }
  TRUE
}

setValidity('Slick', validSlick)


## ---- New ----
newSlick <- function(Title=NULL,
                     Subtitle=NULL,
                     Date=NULL,
                     Author=NULL,
                     Email=NULL,
                     Institution=NULL,
                     Introduction=NULL,
                     MPs=NULL,
                     OMs=NULL,
                     Boxplot=NULL,
                     Kobe=NULL,
                     Quilt=NULL,
                     Spider=NULL,
                     Timeseries=NULL,
                     Tradeoff=NULL) {
  obj <- new('Slick',
             Title,
             Subtitle,
             Date,
             Author,
             Email,
             Institution,
             Introduction,
             MPs,
             OMs,
             Boxplot,
             Kobe,
             Quilt,
             Spider,
             Timeseries,
             Tradeoff)

  methods::validObject(obj)
  obj
}


# ---- Methods ----

#' @describeIn Slick Return `Timeseries` from a [Slick-class()] object
setMethod("Timeseries","Slick",function(Code) Code@Timeseries)

#' @describeIn Slick Assign `Timeseries` to a [Slick-class()] object
setMethod("Timeseries<-","Slick",function(Slick, value) {
  Slick@Timeseries <- value
  methods::validObject(Slick)
  Slick
})



## ---- Check ----
#' @describeIn Check Check [Slick-class()] objects for errors
setMethod('Check', 'Slick', function(object, skip_warnings) {

  ll <- CheckList()
  ll@object <- 'Slick'
  ll@complete <- list()
  ll@empty <- list()

  obj_classes <- c('MPs', 'OMs', 'Boxplot',
                   'Quilt', 'Kobe', 'Spider',
                   'Timeseries', 'Tradeoff')

  for (cl in obj_classes) {
    chk <- Check(slot(object, cl))
    ll@complete[[cl]] <- chk@complete
    ll@empty[[cl]] <- chk@empty
    ll@errors[[cl]] <- chk@errors

  }

  ll
})


## ---- Design ----

#' @param object A [Slick-class()] object
#' @describeIn Slick Access the operating model `Design` matrix
setMethod('Design', 'Slick', function(object) object@OMs@Design)

#' @param object A [Slick-class()] object
#' @describeIn Slick Assign the operating model `Design` matrix
setMethod('Design<-', 'Slick', function(object,value) {
  object@OMs@Design <- value
  methods::validObject(object)
  object
})

## ---- Factors ----

#' @param object A [Slick-class()] object
#' @describeIn Slick Access the operating model `Factors` table
setMethod('Factors', 'Slick', function(object, lang='en')
  get_language(object@OMs@Factors,lang)
)


#' @param object A [Slick-class()] object
#' @describeIn Slick Assign the operating model `Factors` table
setMethod('Factors<-', 'Slick', function(object,value) {
  object@OMs@Factors <- value
  methods::validObject(object)
  object
})




## --- Show ----

#' @describeIn Slick Show objects of class `Slick`
#' @param object An object of class [Slick-class()]
#' @export
setMethod("show", "Slick", function(object) {
  cat('An object of class `Slick` \n\n')
  cat('Title:', Title(object, 'en'), '\n')
  cat('Subtitle:', Subtitle(object, 'en'), '\n')
  cat('Date:', paste(Date(object), collapse=''), '\n')
  cat('Author:', paste(Author(object),collapse=', '), '\n')
  cat('Email:', paste(Email(object),collapse=', '), '\n')
  cat('Institution:', paste(Institution(object),collapse=', '), '\n')

  intro_text <- Introduction(object)

  if (length(intro_text)>0) {
    if (nchar(intro_text) > 50) {
      cat('Introduction:', paste0(substr(Introduction(object), 1, 50), ' ...\n'))
    } else {
      cat('Introduction:', Introduction(object))
    }

  } else {
    cat('Introduction:')
  }

  # MPs
  cat('\n\nMPs:')
  mps <- MPs(object)
  meta <- Metadata(mps)
  if (nrow(meta)<1) {
    cat('None specified.')
  } else {
    cat('\n')
    print(meta)
  }

})


## ---- MPs ----

setMethod("MPs", 'Slick', function(Code) {
  Code@MPs
})

#' @rdname MPs-methods
#' @param object A [Slick-class()] object
#' @param value An [MPs-class()] object
#' @export
setGeneric("MPs<-", function(object, value) standardGeneric("MPs<-"))

setMethod("MPs<-", "Slick", function(object, value) {
  object@MPs <- value
  if (any(nchar(Color(object@MPs)))<1) {
    if (length(object@MPs@Code)>1) {
      object@MPs@Color <- default_mp_colors(length(object@MPs@Code))
    }
  }

  methods::validObject(object)
  object
})

## ---- OMs ----

setMethod("OMs", 'Slick', function(Factors) {
  Factors@OMs
})

#' @rdname OMs-methods
#' @param object A [Slick-class()] object
#' @param value An [OMs-class()] object
#' @export
setGeneric("OMs<-", function(object, value) standardGeneric("OMs<-"))

setMethod("OMs<-", "Slick", function(object, value) {
  object@OMs <- value
  methods::validObject(object)
  object
})






## ---- Boxplot ----

#' @describeIn Boxplot-methods Return `Boxplot` from a [Slick-class()] object
setMethod("Boxplot","Slick",function(Code) Code@Boxplot)

#' @describeIn Boxplot-methods Assign a [Boxplot-class()] object to a [Slick-class()] object
setMethod("Boxplot<-", "Slick", function(Slick, value) {
  Slick@Boxplot <- value
  methods::validObject(Slick)
  Slick
})


## ---- Kobe ----
#' @describeIn Kobe-methods Return `Kobe` from a [Slick-class()] object
setMethod("Kobe","Slick",function(Code) Code@Kobe)

#' @describeIn Kobe-methods Assign a [Kobe-class()] object to a [Slick-class()] object
setMethod("Kobe<-", "Slick", function(Slick, value) {
  Slick@Kobe <- value
  methods::validObject(Slick)
  Slick
})

## ---- Quilt ----

#' @describeIn Quilt-methods Return `Quilt` from a [Slick-class()] object
setMethod("Quilt","Slick",function(Code) Code@Quilt)

#' @describeIn Quilt-methods Assign a [Quilt-class()] object to a [Slick-class()] object
setMethod("Quilt<-", "Slick", function(Slick, value) {
  Slick@Quilt <- value
  methods::validObject(Slick)
  Slick
})

## ---- Spider ----

#' @describeIn Spider-methods Return `Spider` from a [Slick-class()] object
setMethod("Spider","Slick",function(Code) Code@Spider)

#' @describeIn Spider-methods Assign a [Spider-class()] object to a [Slick-class()] object
setMethod("Spider<-", "Slick", function(Slick, value) {
  Slick@Spider <- value
  methods::validObject(Slick)
  Slick
})


## --- Timeseries ----

#' @describeIn Timeseries-methods Return `Timeseries` from a [Slick-class()] object
setMethod("Timeseries","Slick",function(Code) Code@Timeseries)

#' @describeIn Timeseries-methods Assign a [Timeseries-class()] object to a [Slick-class()] object
setMethod("Timeseries<-", "Slick", function(Slick, value) {
  Slick@Timeseries <- value
  methods::validObject(Slick)
  Slick
})


## ---- Tradeoff ----

#' @describeIn Tradeoff-methods Return `Tradeoff` from a [Slick-class()] object
setMethod("Tradeoff","Slick",function(Code) Code@Tradeoff)

#' @describeIn Tradeoff-methods Assign a [Tradeoff-class()] object to a [Slick-class()] object
setMethod("Tradeoff<-", "Slick", function(Slick, value) {
  Slick@Tradeoff <- value
  methods::validObject(Slick)
  Slick
})





## Functions ----

process_markdown <- function(val, markdown) {
  if (markdown) val <- shiny::markdown(val)
  val
}

#' @describeIn Slick Access `Title`, Multi-language support
#' @param lang `r lang_param()`
#' @param markdown Logical. Process markdown?
#' @export
Title <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Title, lang), markdown)
}

#' @describeIn Slick Assign `Title`, Multi-language support
#' @param value The value to assign to the object. See `Slots` for format of the
#' relevant object class
#' @export
`Title<-` <- function(object,  value) {
  object@Title <- value
  methods::validObject(object)
  object
}



#' @describeIn Slick Access `Subtitle`, Multi-language support
#' @export
Subtitle <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Subtitle, lang), markdown)
}

#' @describeIn Slick Assign `Subtitle`, Multi-language support
#' @export
`Subtitle<-` <- function(object,  value) {
  object@Subtitle <- value
  methods::validObject(object)
  object
}

#' @describeIn Slick Access `Date`
#' @export
Date <- function(object) {
  object@Date
}

#' @describeIn Slick Assign `Date`
#' @export
`Date<-` <- function(object, value) {
  if (is.null(value)) return(object)
  if (inherits(value, 'POSIXct'))
    value <- value |> as.Date() |> as.character()

  object@Date <- value
  methods::validObject(object)
  object
}


#' @describeIn Slick Access `Author`
#' @export
Author <- function(object, markdown=FALSE) {
  process_markdown(object@Author, markdown)
}

#' @describeIn Slick Assign `Author`
#' @export
`Author<-` <- function(object,  value) {
  object@Author <- value
  methods::validObject(object)
  object
}


#' @describeIn Slick Access `Email`
#' @export
Email <- function(object, markdown=FALSE) {
  process_markdown(object@Email, markdown)
}

#' @describeIn Slick Assign `Email`
#' @export
`Email<-` <- function(object,  value) {
  object@Email <- value
  methods::validObject(object)
  object
}


#' @describeIn Slick Access `Institution`
#' @export
Institution <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Institution, lang), markdown)
}

#' @describeIn Slick Assign `Institution`
#' @export
`Institution<-` <- function(object,  value) {
  object@Institution <- value
  methods::validObject(object)
  object
}

#' @describeIn Slick Access `Introduction`
#' @export
Introduction <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Introduction, lang), markdown)
}

#' @describeIn Slick Assign `Introduction`, can include Markdown. See `Examples`
#' @export
`Introduction<-` <- function(object,  value) {
  object@Introduction <- value
  methods::validObject(object)
  object
}




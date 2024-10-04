
# ---- Class ----

#' Create a `Slick` class object
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
#' [Spider()], [Timeseries()], [Tradeoff()], [Check()],
#' [Title()], [Subtitle()], [Date()], [Author()], [Email()],
#' [Institution()], [Introduction()]
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
setClass("Slick",
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
  chk <- Check(object)
  if (is.list(chk@empty)) {
    for (i in seq_along(chk@errors)) {
     if (length(chk@errors[[i]])>0) {
       return(chk@errors[[i]])
     }
      if(all(unlist(chk@empty)))
        return(TRUE)
    }
  } else {
    if (chk@empty) return(TRUE)
    if (length(chk@errors)>0) return(chk@errors)
  }


  TRUE
}

setValidity('Slick', validSlick)


## ---- New ----

#' @describeIn Slick Create a [Slick-class()] object
#' @param Title `r title_param()`
#' @param Subtitle `r subtitle_param()`
#' @param Date `r date_param()`
#' @param Author `r author_param()`
#' @param Email `r email_param()`
#' @param Institution `r institution_param()`
#' @param Introduction `r introduction_param()`
#' @param MPs An object of class [MPs-class()]
#' @param OMs An object of class [OMs-class()]
#' @param Boxplot An object of class [Boxplot-class()]
#' @param Kobe An object of class [Kobe-class()]
#' @param Quilt An object of class [Quilt-class()]
#' @param Spider An object of class [Spider-class()]
#' @param Timeseries An object of class [Timeseries-class()]
#' @param Tradeoff An object of class [Tradeoff-class()]
#' @export
Slick <- function(Title='',
                  Subtitle='',
                  Date=Sys.Date(),
                  Author='',
                  Email='',
                  Institution='',
                  Introduction='',
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



## ---- Check ----




#' @describeIn Check Check [Slick-class()] objects for errors
setMethod('Check', 'Slick', function(object) {

  object <- Update(object)
  # check metadata stuff
  ll <- CheckList()
  ll@object <- 'Slick'
  ll@complete <- list()
  ll@empty <- list()


  ll@errors$Title <- unlist(check_lang_list(object@Title))
  ll@errors$Subtitle <- unlist(check_lang_list(object@Subtitle))
  ll@errors$Introduction <- unlist(check_lang_list(object@Introduction))

  obj_classes <- c('MPs', 'OMs', 'Boxplot',
                   'Quilt', 'Kobe', 'Spider',
                   'Timeseries', 'Tradeoff')

  nOM <- nrow(object@OMs@Design)
  mps <- Code(MPs(object))
  nMPs <- ifelse(all(nchar(mps)<1), 0, length(mps))

  for (cl in obj_classes) {
    req_dims <- value_dimensions(cl)
    if (!is.null(req_dims)) {
      check_dims <- rep(NA, length(req_dims))
      if (nOM>0) {
        check_dims[match('nOM',req_dims)] <- nOM
      }
      if (nMPs>0) {
        check_dims[match('nMP',req_dims)] <- nMPs
      }

      chk <- Check(slot(object, cl), check_dims)
    } else {
      chk <- Check(slot(object, cl))
    }

    ll@complete[[cl]] <- chk@complete
    ll@empty[[cl]] <- chk@empty
    ll@errors[[cl]] <- chk@errors
    ll@warnings[[cl]] <- chk@warnings
  }
  ll
})


## ---- Design ----

#' @param object A [Slick-class()] object
#' @describeIn Design Access the operating model `Design` matrix from a [Slick-class()] object
setMethod('Design', 'Slick', function(object) object@OMs@Design)

#' @param object A [Slick-class()] object
#' @describeIn Design Assign the operating model `Design` matrix to a [Slick-class()] object
setMethod('Design<-', 'Slick', function(object,value) {
  object@OMs@Design <- value
  methods::validObject(object)
  object
})

## ---- Factors ----

#' @param object A [Slick-class()] object
#' @describeIn Factors Access the operating model `Factors` table from a [Slick-class()] object
setMethod('Factors', 'Slick', function(object, lang='en')
  get_language(object@OMs@Factors,lang)
)


#' @param object A [Slick-class()] object
#' @describeIn Factors Assign the operating model `Factors` table to a [Slick-class()] object
setMethod('Factors<-', 'Slick', function(object,value) {
  object@OMs@Factors <- value
  methods::validObject(object)
  object
})



## --- Show ----


#' @describeIn show Print a [Slick-class()] object
setMethod("show", "Slick", function(object) {

  chk <- print_show_heading(object)

  print_metadata(object@Title, 'Title', addNum = FALSE)
  print_metadata(object@Subtitle, 'Subtitle', addNum = FALSE)
  print_metadata(object@Date, 'Date', addNum = FALSE)
  print_metadata(object@Author, 'Author')
  print_metadata(object@Email, 'Email')
  print_metadata(object@Institution, 'Institution')
  print_metadata(object@Introduction, 'Introduction', addNum = FALSE)


  obj_classes <- c('MPs', 'OMs', 'Boxplot',
                   'Quilt', 'Kobe', 'Spider',
                   'Timeseries', 'Tradeoff')

  for (cl in obj_classes) {
    temp <- slot(object,cl)
    chk <-  Check(temp)
    cli::cli_h2('{.code {chk@object}}')
    if (chk@empty) {
      cli::cli_alert_info('Object is empty')
    } else {
      if (length(chk@errors)>0)
        print_errors(chk@errors)
      if (length(chk@warnings)>0)
        print_warnings(chk@warnings)
      if (chk@complete)
        cli::cli_alert_success('Complete')
    }
  }

})


## ---- Metadata ----
#' @describeIn Metadata Return `Author`, `Email`, and `Institution` from [Slick()] objects
#' @export
setMethod('Metadata', 'Slick', function(object, lang='en') {

  data.frame(Author=object@Author,
             Email=object@Email,
             Institution=object@Institution)

})

## ---- MPs ----
#' @describeIn MPs-methods Return an [MPs-class()] object from a [Slick()] object
setMethod("MPs", 'Slick', function(Code) {
  Code@MPs
})


#' @describeIn MPs-methods Assign an [MPs-class()] object to a [Slick()] object
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
#' @describeIn OMs-methods Return an [OMs-class()] object from a [Slick()] object
setMethod("OMs", 'Slick', function(Factors) {
  Factors@OMs
})

#' @describeIn OMs-methods Assign an [OMs-class()] object to a [Slick()] object
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





## Slick Object Functions ----

process_markdown <- function(val, markdown) {
  if (markdown) val <- shiny::markdown(val)
  val
}



#' @describeIn Slick-class Access `Title`, Multi-language support
#' @param object A [Slick-class()] object
#' @param lang `r lang_param()`
#' @param markdown Logical. Process markdown?
#'
#' @export
Title <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Title, lang), markdown)
}

#' @describeIn Slick-class Assign `Title`, Multi-language support
#' @param value The value to assign to the object. See `Slots` for format of the
#' relevant object class
#' @export
`Title<-` <- function(object,  value) {
  object@Title <- value
  methods::validObject(object)
  object
}



#' @describeIn Slick-class Access `Subtitle`, Multi-language support
#' @export
Subtitle <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Subtitle, lang), markdown)
}

#' @describeIn Slick-class Assign `Subtitle`, Multi-language support
#' @export
`Subtitle<-` <- function(object,  value) {
  object@Subtitle <- value
  methods::validObject(object)
  object
}

#' @describeIn Slick-class Access `Date`
#' @export
Date <- function(object) {
  object@Date
}

#' @describeIn Slick-class Assign `Date`
#' @export
`Date<-` <- function(object, value) {
  if (is.null(value)) return(object)
  if (inherits(value, 'POSIXct'))
    value <- value |> as.Date() |> as.character()

  object@Date <- value
  methods::validObject(object)
  object
}


#' @describeIn Slick-class Access `Author`
#' @export
Author <- function(object, markdown=FALSE) {
  process_markdown(object@Author, markdown)
}

#' @describeIn Slick-class Assign `Author`
#' @export
`Author<-` <- function(object,  value) {
  object@Author <- value
  methods::validObject(object)
  object
}


#' @describeIn Slick-class Access `Email`
#' @export
Email <- function(object, markdown=FALSE) {
  process_markdown(object@Email, markdown)
}

#' @describeIn Slick-class Assign `Email`
#' @export
`Email<-` <- function(object,  value) {
  object@Email <- value
  methods::validObject(object)
  object
}


#' @describeIn Slick-class Access `Institution`
#' @export
Institution <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Institution, lang), markdown)
}

#' @describeIn Slick-class Assign `Institution`
#' @export
`Institution<-` <- function(object,  value) {
  object@Institution <- value
  methods::validObject(object)
  object
}

#' @describeIn Slick-class Access `Introduction`
#' @export
Introduction <- function(object, lang='en', markdown=FALSE) {
  process_markdown(get_language(object@Introduction, lang), markdown)
}

#' @describeIn Slick-class Assign `Introduction`, can include Markdown. See `Examples`
#' @export
`Introduction<-` <- function(object,  value) {
  object@Introduction <- value
  methods::validObject(object)
  object
}





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


# ---- Slick Class ----

#' `Slick` S4 class and functions
#'
#' Each slot of the `Slick` object can be accessed or modified by the corresponding
#' accessor function. See Examples sections below for more details.
#'
#' @slot Title Title for the `Slick` object. A character string. For multiple languages,
#' use a named list with names: `en`, `es`, `fr` for the three supported languages.
#'
#' @slot Subtitle Subtitle for the `Slick` object. A character string or a named list with
#' languages: `en`, `es`, `fr`
#'
#' @slot Date Either an object of class `Date` or character string.
#' Date the Slick object was created. Defaults to `Sys.Date()`
#'
#' @slot Author A character vector with name(s) of author(s) for the `Slick` object
#' @slot Email A character vector with email addresses for the author(s). Optional.
#' Must be same length as `Author`
#' @slot Institution A character vector with institution details for the author(s). Optional.
#' Must be same length as `Author`
#'
#' @slot Introduction Introduction text for the `Slick` object. Supports all markdown formatting.
#' For multiple languages, use a named list with names: `en`, `es`, `fr` for the
#' three supported languages.
#'
#' @slot MPs An object of class [MPs()]
#' @slot OMs An object of class [OMs()]
#' @slot Boxplot An object of class [Boxplot()]
#' @slot Kobe An object of class [Kobe()]
#' @slot Quilt An object of class [Quilt()]
#' @slot Spider An object of class [Spider()]
#' @slot Timeseries An object of class [Timeseries()]
#' @slot Tradeoff An object of class [Tradeoff()]
#'
#'
#' @usage Slick()
#' @include class_Boxplot.R
#' @include class_MPs.R
#' @include class_OMs.R
#' @include class_Quilt.R
#' @include class_Spider.R
#' @include class_Kobe.R
#' @include class_Timeseries.R
#' @rdname Slick
#'
#' @examples
#' mySlick <- Slick()
#' Title(mySlick) <- 'This is the title'
#' Subtitle(mySlick) <- 'This is the subtitle'
#' Date(mySlick) <- Sys.Date()
#'
#' Title(mySlick)
#' Subtitle(mySlick)
#' Date(mySlick)
#'
#'
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


setMethod("initialize", "Slick", function(.Object,
                                          Title=NULL,
                                          Subtitle=NULL,
                                          Date=Sys.Date(),
                                          Author=NULL,
                                          Email=NULL,
                                          Institution=NULL,
                                          Introduction=NULL,
                                          MPs=NULL,
                                          OMs=newOMs(),
                                          Boxplot=newBoxplot(),
                                          Kobe=newKobe(),
                                          Quilt=newQuilt(),
                                          Spider=newSpider(),
                                          Timeseries=newTimeseries(),
                                          Tradeoff=newTradeoff()) {

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

showSlick <- function(slick) {
  cat('An object of class `Slick` \n\n')
  cat('Title:', Title(slick, 'en'), '\n')
  cat('Subtitle:', Subtitle(slick, 'en'), '\n')
  cat('Date:', paste(Date(slick), collapse=''), '\n')
  cat('Author:', paste(Author(slick),collapse=', '), '\n')
  cat('Email:', paste(Email(slick),collapse=', '), '\n')
  cat('Institution:', paste(Institution(slick),collapse=', '), '\n')

  intro_text <- Introduction(slick)

  if (length(intro_text)>0) {
    if (nchar(intro_text) > 50) {
      cat('Introduction:', paste0(substr(Introduction(slick), 1, 50), ' ...\n'))
    } else {
      cat('Introduction:', Introduction(slick))
    }

  } else {
    cat('Introduction:')
  }

  # MPs
  cat('\n\nMPs:')
  MPs <- MPs(slick)
  meta <- Metadata(MPs)
  if (nrow(meta)<1) {
    cat('None specified.')
  } else {
    cat('\n')
    print(meta)
  }


}

# ---- Boxplot ----
setMethod("Boxplot","Slick",function(Code) Code@Boxplot)


#' @rdname Boxplot-methods
#' @param Slick An object of class [Slick-class()]
#' @param value An object of class [Boxplot-class()]
#' @export
setGeneric("Boxplot<-", function(Slick, value) standardGeneric("Boxplot<-"))


setMethod("Boxplot<-", "Slick", function(Slick, value) {
  Slick@Boxplot <- value
  methods::validObject(Slick)
  Slick
})

# ---- Kobe ----
setMethod("Kobe","Slick",function(Code) Code@Kobe)


#' @rdname Kobe-methods
#' @param Slick An object of class [Slick-class()]
#' @param value An object of class [Kobe-class()]
#' @export
setGeneric("Kobe<-", function(Slick, value) standardGeneric("Kobe<-"))


setMethod("Kobe<-", "Slick", function(Slick, value) {
  Slick@Kobe <- value
  methods::validObject(Slick)
  Slick
})


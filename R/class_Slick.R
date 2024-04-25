
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

#' Create a `Slick` object
#'
#' Each slot of the `Slick` object can be accessed or modified by the corresponding
#' accessor function. See Examples sections below for more details.
#'
#' @param Title Title for the `Slick` object. A character string. For multiple languages,
#' use a named list with names: `en`, `es`, `fr` for the three supported languages.
#'
#' @param Subtitle Subtitle for the `Slick` object. A character string or a named list with
#' languages: `en`, `es`, `fr`
#'
#' @param Date Either an object of class `Date` or character string.
#' Date the Slick object was created. Defaults to `Sys.Date()`
#'
#' @param Author A character vector with name(s) of author(s) for the `Slick` object
#' @param Email A character vector with email addresses for the author(s). Optional.
#' Must be same length as `Author`
#' @param Institution A character vector with institution details for the author(s). Optional.
#' Must be same length as `Author`
#'
#' @param Introduction Introduction text for the `Slick` object. Supports all markdown formatting.
#' For multiple languages, use a named list with names: `en`, `es`, `fr` for the
#' three supported languages.
#'
#' @param MPs An object of class [MPs()]
#' @param OMs An object of class [OMs()]
#' @param Boxplot An object of class [Boxplot()]
#' @param Kobe An object of class [Kobe()]
#' @param Quilt An object of class [Quilt()]
#' @param Spider An object of class [Spider()]
#' @param Timeseries An object of class [Timeseries()]
#' @param Tradeoff An object of class [Tradeoff()]
#'
#'
#' @usage Slick()
#' @author A. Hordyk
#'
#' @include class_MPs.R
#' @include class_OMs.R
#' @include class_Quilt.R
#' @include class_Spider.R
#' @include class_Boxplot.R
#' @include class_Kobe.R
#' @include class_Timeseries.R
#' @rdname Slick
#'
#' @examples
#' mySlick <- Slick()
#' Title(mySlick) <- 'This is the title'#'
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

  # OMs

  ## metadata

  ## design
#
#   obj <- Metadata(OMs(object))
#   if (inherits(obj, 'data.frame')) {
#     test <- check_data.frame(obj,
#                              req=c('Factor', 'Level', 'Description'),
#                              opt=c('Set'))
#     if (!is.null(test)) return(test)
#   }
#   if (inherits(obj, 'list')) {
#     test <- lapply(obj, check_data.frame, req=c('Factor', 'Level', 'Description'),
#                    opt=c('Set'))
#     if (!all(unlist(lapply(test, is.null)))) return(test)
#   }


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

  validObject(obj)
  obj
}


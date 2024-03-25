


# ---- SlickData Class ----

#' Class \code{'SlickData'}
#'
#' A `Slick` Data object for uploading into the Slick App.
#'
#'
#' @author A. Hordyk
#' @include class_OMs.R
#' @include class_MPs.R
#' @include class_Quilt.R
#' @include class_Spider.R
#' @include class_Boxplot.R
#' @include class_Kobe.R
#' @include class_Timeseries.R
#' @keywords classes
#' @docType class
#' @export
#'
SlickData <- setClass("SlickData",
         slots=c(Title='character_list',
                 Subtitle='character_list',
                 Fishery='character_list',
                 Introduction='list',
                 Date='character',
                 Author='character_list',
                 Email='character_list',
                 Institution='character_list',
                 OMs='OMs',
                 MPs='MPs',
                 Quilt='Quilt',
                 Spider='Spider',
                 Boxplot='Boxplot',
                 Kobe='Kobe',
                 Timeseries='Timeseries')
)


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
                                              Timeseries=NULL) {

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
  .Object@Timeseries <- use_ifnot_NULL('Timeseries', Timeseries, .Object)
  .Object
})


validSlickData <- function(object) {
  errors <- list()
  if (length(errors)>0)
    return(errors)
  TRUE
}

setValidity('SlickData', validSlickData)


newSlickData <- function(Title=NULL,
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
                     Timeseries=NULL) {
  SlickData <- new('SlickData', Title,
                   Subtitle,
                   Fishery,
                   Introduction,
                   Date,
                   Author, Email, Institution,
                   OMs, MPs, Quilt,
                   Spider, Boxplot,Kobe, Timeseries)
  validObject(SlickData)
  SlickData
}


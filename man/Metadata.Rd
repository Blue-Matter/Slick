% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa_generics.R, R/class_Boxplot.R,
%   R/class_Kobe.R, R/class_MPs.R, R/class_Quilt.R, R/class_Tradeoff.R,
%   R/class_Timeseries.R, R/class_Spider.R, R/class_Slick.R
\name{Metadata}
\alias{Metadata}
\alias{Metadata<-}
\alias{Metadata,Boxplot-method}
\alias{Metadata<-,Boxplot-method}
\alias{Metadata,Kobe-method}
\alias{Metadata<-,Kobe-method}
\alias{Metadata,MPs-method}
\alias{Metadata<-,MPs-method}
\alias{Metadata,Quilt-method}
\alias{Metadata<-,Quilt-method}
\alias{Metadata,Tradeoff-method}
\alias{Metadata<-,Tradeoff-method}
\alias{Metadata,Timeseries-method}
\alias{Metadata<-,Timeseries-method}
\alias{Metadata,Spider-method}
\alias{Metadata<-,Spider-method}
\alias{Metadata,Slick-method}
\title{Return \code{Code}, \code{Label}, \code{Description} and other information from an object}
\usage{
Metadata(object, lang = "en")

Metadata(object) <- value

\S4method{Metadata}{Boxplot}(object, lang = "en")

\S4method{Metadata}{Boxplot}(object) <- value

\S4method{Metadata}{Kobe}(object, lang = "en")

\S4method{Metadata}{Kobe}(object) <- value

\S4method{Metadata}{MPs}(object, lang = "en")

\S4method{Metadata}{MPs}(object) <- value

\S4method{Metadata}{Quilt}(object, lang = "en")

\S4method{Metadata}{Quilt}(object) <- value

\S4method{Metadata}{Tradeoff}(object, lang = "en")

\S4method{Metadata}{Tradeoff}(object) <- value

\S4method{Metadata}{Timeseries}(object, lang = "en")

\S4method{Metadata}{Timeseries}(object) <- value

\S4method{Metadata}{Spider}(object, lang = "en")

\S4method{Metadata}{Spider}(object) <- value

\S4method{Metadata}{Slick}(object, lang = "en")
}
\arguments{
\item{object}{A \code{\link[=Slick-class]{Slick-class()}}, \code{\link[=MPs-class]{MPs-class()}}, \code{\link[=Boxplot-class]{Boxplot-class()}}, \code{\link[=Kobe-class]{Kobe-class()}},
\code{\link[=Quilt-class]{Quilt-class()}}, \code{\link[=Spider-class]{Spider-class()}}, \code{\link[=Timeseries-class]{Timeseries-class()}}, or \code{\link[=Tradeoff-class]{Tradeoff-class()}} object}

\item{lang}{Optional text string specifying the language (if available).
Either 'en', 'es', or 'fr' for English, Spanish, or French respectively}

\item{value}{Replacement value for \code{Metadata()} in the corresponding \code{object}.
See help documentation for the relevant object class for details.}
}
\value{
\verb{A data.frame}

A \code{data.frame}
}
\description{
Return \code{Code}, \code{Label}, \code{Description} and other information from an object
}
\section{Methods (by class)}{
\itemize{
\item \code{Metadata(Boxplot)}: Return Metadata for \code{\link[=Boxplot-class]{Boxplot-class()}} objects

\item \code{Metadata(Boxplot) <- value}: Assign Metadata for \code{\link[=Boxplot-class]{Boxplot-class()}} objects

\item \code{Metadata(Kobe)}: Return Metadata for \code{\link[=Kobe-class]{Kobe-class()}} objects

\item \code{Metadata(Kobe) <- value}: Assign Metadata for \code{\link[=Kobe-class]{Kobe-class()}} objects

\item \code{Metadata(MPs)}: Return Metadata for \code{\link[=MPs-class]{MPs-class()}} objects

\item \code{Metadata(MPs) <- value}: Assign Metadata for \code{\link[=MPs-class]{MPs-class()}} objects

\item \code{Metadata(Quilt)}: Return Metadata for \code{\link[=Quilt-class]{Quilt-class()}} objects

\item \code{Metadata(Quilt) <- value}: Assign Metadata for \code{\link[=Quilt-class]{Quilt-class()}} objects

\item \code{Metadata(Tradeoff)}: Return Metadata for \code{\link[=Tradeoff-class]{Tradeoff-class()}} objects

\item \code{Metadata(Tradeoff) <- value}: Assign Metadata for \code{\link[=Tradeoff-class]{Tradeoff-class()}} objects

\item \code{Metadata(Timeseries)}: Return Metadata for \code{\link[=Timeseries-class]{Timeseries-class()}} objects

\item \code{Metadata(Timeseries) <- value}: Assign Metadata for \code{\link[=Timeseries-class]{Timeseries-class()}} objects

\item \code{Metadata(Spider)}: Return Metadata for \code{\link[=Spider-class]{Spider-class()}} objects

\item \code{Metadata(Spider) <- value}: Assign Metadata for \code{\link[=Spider-class]{Spider-class()}} objects

\item \code{Metadata(Slick)}: Return \code{Author}, \code{Email}, and \code{Institution} from \code{\link[=Slick]{Slick()}} objects

}}

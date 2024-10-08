% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa_generics.R, R/class_Kobe.R,
%   R/class_Timeseries.R
\name{Time}
\alias{Time}
\alias{Time<-}
\alias{Time,Kobe-method}
\alias{Time<-,Kobe-method}
\alias{Time,Timeseries-method}
\alias{Time<-,Timeseries-method}
\title{Access or assign \code{Time} for object of class \code{Kobe} or \code{Timeseries}}
\usage{
Time(object)

Time(object) <- value

\S4method{Time}{Kobe}(object)

\S4method{Time}{Kobe}(object) <- value

\S4method{Time}{Timeseries}(object)

\S4method{Time}{Timeseries}(object) <- value
}
\arguments{
\item{object}{A \code{\link[=Kobe-class]{Kobe-class()}} or \code{\link[=Timeseries]{Timeseries()}} class object}

\item{value}{Value to assign to \code{Time}}
}
\description{
Access or assign \code{Time} for object of class \code{Kobe} or \code{Timeseries}
}
\section{Methods (by class)}{
\itemize{
\item \code{Time(Kobe)}: Return \code{Time} from a \code{\link[=Kobe-class]{Kobe-class()}} object

\item \code{Time(Kobe) <- value}: Assign \code{Time} to a \code{\link[=Kobe-class]{Kobe-class()}} object

\item \code{Time(Timeseries)}: Return \code{Time} from a \code{\link[=Timeseries-class]{Timeseries-class()}} object

\item \code{Time(Timeseries) <- value}: Assign \code{Time} to a \code{\link[=Timeseries-class]{Timeseries-class()}} object

}}
\section{Functions}{
\itemize{
\item \code{Time(object) <- value}: Assign \code{value} to \code{object@Time}

}}

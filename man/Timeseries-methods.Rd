% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa_generics.R, R/class_Timeseries.R,
%   R/class_Slick.R
\docType{methods}
\name{Timeseries}
\alias{Timeseries}
\alias{Timeseries<-}
\alias{Timeseries,missing-method}
\alias{Timeseries,character-method}
\alias{Timeseries,list-method}
\alias{Timeseries,Slick-method}
\alias{Timeseries<-,Slick-method}
\title{Methods for Creating, Accessing and Assigning \code{Timeseries} objects}
\usage{
Timeseries(
  Code = "",
  Label = "",
  Description = "",
  Time = numeric(),
  TimeNow = numeric(),
  TimeLab = "Year",
  Value = array(),
  Preset = list(),
  Target = NULL,
  Limit = NULL,
  RefPoints = list()
)

Timeseries(Slick) <- value

\S4method{Timeseries}{missing}()

\S4method{Timeseries}{character}(
  Code = "",
  Label = "",
  Description = "",
  Time = numeric(),
  TimeNow = numeric(),
  TimeLab = "Year",
  Value = array(),
  Preset = list(),
  Target = NULL,
  Limit = NULL,
  RefPoints = list()
)

\S4method{Timeseries}{list}(
  Code = "",
  Label = "",
  Description = "",
  Time = numeric(),
  TimeNow = numeric(),
  TimeLab = "Year",
  Value = array(),
  Preset = list(),
  Target = NULL,
  Limit = NULL,
  RefPoints = list()
)

\S4method{Timeseries}{Slick}(Code)

\S4method{Timeseries}{Slick}(Slick) <- value
}
\arguments{
\item{Code}{A \emph{short} code for the Performance Indicators for this object.
A character string length \code{nPI} or a named list for multi-language support. See \code{Details}}

\item{Label}{A short label for the Performance Indicators for this object. Used to label axes on charts.
Can be longer than \code{Code} but recommended to keep short as possible so it shows clearly in plots and tables.
A character string length \code{nPI} or a named list for multi-language support. See \code{Details}}

\item{Description}{A description for the Performance Indicators for this object.
Can include Markdown, see \code{Examples}.
A character string length \code{nPI} or a named list for multi-language support. See \code{Details}}

\item{Time}{A numeric vector with values for the historical and projection time-steps. Must
match length \code{nTS} in \code{Value}}

\item{TimeNow}{A numeric value matching the last historical timestep in \code{Time}}

\item{TimeLab}{Character string length 1. Name of the time step (e.g., 'Year').
Will be used as the label in the \code{Timeseries} plot. Use a named list for
multiple languages.}

\item{Value}{A numeric array with the stochastic performance indicator values for each
simulation (sim), operating model (OM), management procedure (MP),  performance indicator (PI),
and historical + projection timestep (nTS).
Dimensions: c(\code{nsim}, \code{nOM}, \code{nMP}, \code{nPI}, \code{nTS})}

\item{Preset}{An optional named list for the preset buttons in the \code{\link[=App]{App()}}. The name of the list
element will appear as a button in the \code{\link[=App]{App()}}.}

\item{Target}{Numeric vector length \code{nPI} with the target value for the PIs.}

\item{Limit}{Numeric vector length \code{nPI} with the limit value for the PIs.}

\item{RefPoints}{List for setting custom Reference Points. Overrides \code{Target} and \code{Limit}.
See \code{Details}}

\item{Slick}{A \code{\link[=Slick-class]{Slick-class()}} object}

\item{value}{A \code{\link[=Timeseries-class]{Timeseries-class()}} object}
}
\description{
An object of class \code{Timeseries} contains information for the Time Series chart.
The \code{Timeseries} function is used both to create and modify an \code{\link[=Timeseries-class]{Timeseries-class()}} object,
and to access and assign \code{Timeseries} for an object of class \code{\link[=Slick-class]{Slick-class()}}.
See \code{Details}.
}
\details{
Use \code{\link[=plotTimeseries]{plotTimeseries()}} to create the boxplot from the console.
\subsection{Note}{

Character strings in \code{Code}, \code{Label}, and \code{Description} must all be same length
as the number of performance indicators (\code{nPIs}) in \code{Value}

Objects of class \code{Timeseries} are created with \code{Timeseries()}
}

\subsection{Multi-Language Support}{

Text with multi-language supported can be provided as a named list. Available languages:
\itemize{
\item \code{en}: English (default)
\item \code{es}: Spanish
\item \code{fr}: French
}
}

\subsection{Custom Reference Points with \code{RefPoints}}{

\code{RefPoints} provides more options than the default \code{Target} and \code{Limit} reference points. It can be used to control the name and
color of the reference point lines, or to add additional reference point lines to the \code{Timeseries} plot.

Note: If \code{RefPoints} is included, \code{Target} and \code{Limit} are ignored.

\code{RefPoints} must be a list of length <= \code{length(Code)} (i.e., the number of performance indicators).
Each element in \code{RefPoints} should be a named list:
\itemize{
\item \code{Name} character vector with name(s) of reference point(s)
\item \code{Value} numeric vector length \code{Name} with value(s) for the reference point(s)
\item \code{Color} character vector length \code{Name} with color(s) for the reference point(s)
}

See \code{Examples}
}

\subsection{Accessing Slots}{

Use the  \code{\link[=Code]{Code()}}, \code{\link[=Label]{Label()}}, \code{\link[=Description]{Description()}}, \code{\link[=Value]{Value()}}, \code{\link[=Preset]{Preset()}} functions to access and assign the values for an
existing \code{Timeseries} object, see \code{Examples}
}
}
\section{Functions}{
\itemize{
\item \code{Timeseries(missing)}: Create an empty \code{Timeseries} object

\item \code{Timeseries(character)}: Create a populated \code{Timeseries} object

\item \code{Timeseries(list)}: Create a populated \code{Timeseries} object

\item \code{Timeseries(Slick)}: Return \code{Timeseries} from a \code{\link[=Slick-class]{Slick-class()}} object

\item \code{Timeseries(Slick) <- value}: Assign a \code{\link[=Timeseries-class]{Timeseries-class()}} object to a \code{\link[=Slick-class]{Slick-class()}} object

}}
\examples{
# Generate dummy values
nsim <- 10
nOM <- 2
nMP <- 4
nPI <- 3
nHistTS <- 50
nProjTS <- 30
nTS <- nHistTS + nProjTS

set.seed(101)

values <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))

pi_means <- c(1,1, 1000)

for (om in 1:nOM) {
  for (pi in 1:nPI) {
    # PI identical for historical
    histVals <- matrix(
      pi_means[pi] *
        cumprod(c(rlnorm(nHistTS*nsim, 0, 0.05))),
      nrow=nsim, ncol=nHistTS, byrow=TRUE)
    histVals <- replicate(nMP, histVals)
    values[,om, , pi,1:nHistTS] <- aperm(histVals, c(1,3,2))

    for (mp in 1:nMP) {
      values[,om, mp, pi,(nHistTS+1):nTS] <- matrix(
        pi_means[pi] *
          cumprod(c(rlnorm(nProjTS*nsim, 0, 0.05))),
        nrow=nsim, ncol=nProjTS, byrow=FALSE)
    }
  }
}

# Create and populate Object
timeseries <- Timeseries(Code=c('B/BMSY', 'F/FMSY', 'TAC'),
                         Label=c('B/BMSY',
                                 'F/FMSY',
                                 'TAC'),
                         Description = c('This is the description for PI 1',
                                         'This is the description for PI 2',
                                         'This is the description for PI 3'),
                         Value=values
)

# Last historical time step
TimeNow(timeseries) <- 2024

# Add values for time steps
Time(timeseries) <- c(rev(seq(TimeNow(timeseries), by=-1, length.out=nHistTS)),
                      seq(TimeNow(timeseries)+1, by=1, length.out=nProjTS))

# Check
Check(timeseries)

# Add to `Slick` object
slick <- Slick()
Timeseries(slick) <- timeseries

# Plots
plotTimeseries(slick)
plotTimeseries(slick, 2)
plotTimeseries(slick, 3)

plotTimeseries(slick, byMP=TRUE)

plotTimeseries(slick, byOM=TRUE)

# Custom Reference Points
RefPoints(timeseries) <- list(
  list(Name=c('0.5 BMSY', 'BMSY', '1.5 BMSY'),
       Value=c(0.5, 1, 1.5),
       Color=c('red', 'orange', 'green')),
  list(Name=c('0.8 FMSY', 'FMSY'),
       Value=c(0.8,1),
       Color=c('orange', 'red')),
  list(Name='Target Catch',
       Value=1200,
       Color='blue')
)

Timeseries(slick) <- timeseries
plotTimeseries(slick)
plotTimeseries(slick, 2)
plotTimeseries(slick, 3)


}
\seealso{
\code{\link[=Timeseries-class]{Timeseries-class()}}, \code{\link[=Code]{Code()}}, \code{\link[=Label]{Label()}}, \code{\link[=Description]{Description()}},
\code{\link[=Metadata]{Metadata()}}, \code{\link[=Value]{Value()}}, \code{\link[=Preset]{Preset()}}, \code{\link[=plotTimeseries]{plotTimeseries()}}

\code{\link[=Code]{Code()}}, \code{\link[=Label]{Label()}}, \code{\link[=Description]{Description()}}, \code{\link[=Metadata]{Metadata()}}, \code{\link[=Value]{Value()}}, \code{\link[=Preset]{Preset()}}
}

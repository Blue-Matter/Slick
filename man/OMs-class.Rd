% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_OMs.R
\docType{class}
\name{OMs-class}
\alias{OMs-class}
\title{\code{OMs} S4 class and functions}
\description{
An object of class \code{OMs} contains information about the operating models (MPs)
in the \code{\link[=Slick]{Slick()}} object. Like all S4 objects in \code{Slick}, slots in this
object can be accessed and assigned using functions corresponding to slot name.
See \code{\link[=OMs]{OMs()}} and the the \verb{See Also} section below.
}
\details{
\subsection{Multi-Language Support}{

Use a named list to use multi-languages in \code{Factors}
}

\subsection{Factors}{

\code{Factors} can be accessed and assigned using \code{Factors(myslick)} and
\code{Factors(myslick) <- data.frame()} respectively.

The \code{Factor} column should be character strings with the name of each factor,
while the \code{Level} column is a \code{numeric} or \code{character} value with the level for the
corresponding factor.

The \code{Description} column is a description for each row, i.e., a unique factor and level.
See \code{Examples}.
}

\subsection{Design}{

The \code{Design} matrix is \code{nOM} rows and \code{nFactor} columns. The values in each column should
either be \code{numeric} values indicating the levels for the corresponding factor,
or the actual level values (i.e., \code{Factors$Level}) that correspond to each OM. See \code{Examples}.
}

\subsection{Preset}{

For \code{OMs} objects, \code{Preset} should be a named list, where each list element represents a
different preset button to be shown in the app by the name of the list element, and
each named list element should be a list of length \code{nFactors}, with the list elements
for each Factor containing numeric values indicating the levels to include for that factor.
See \code{Examples}
}
}
\section{Slots}{

\describe{
\item{\code{Factors}}{A \code{data.frame} with column headings \code{Factor}, \code{Level}, and \code{Description}. See \code{Details}}

\item{\code{Design}}{A \code{data.frame} with \code{nFactor} columns (i.e., \code{length(unique(Factors$Factor))}), and \code{nOM}
rows. See \code{Details}}

\item{\code{Preset}}{An optional named list for the preset buttons in the \code{\link[=App]{App()}}. The name of the list
element will appear as a button in the \code{\link[=App]{App()}}.. See \code{Details} and \code{Examples}}
}}

\examples{

# Create Object
oms <- OMs()

# Specify Factors
Factors(oms) <- data.frame(Factor='M',
                           Level=c('Base', 'Low M', 'High M'),
                           Description=c('Base Case',
                                         'Lower Natural Mortality',
                                         'Higher Natural Mortality')
)

Factors(oms)

# OM Design

Design(oms) <- data.frame(M=c('Base', 'Low M', 'High M'))

# Add names for OMs
rownames(Design(oms)) <- c('Base Case', 'Less Productive', 'More Productive')

Design(oms)

# Preset

Preset(oms) <- list('Base Case'=list(1),
                    'Low M' = list(2),
                    'High M' = list(3),
                    'All'= list(1:3)
)

# Create Slick Object
myslick <- Slick()

# Add OMs to Slick Object
OMs(myslick) <- oms
}
\seealso{
\code{\link[=OMs]{OMs()}}, \code{\link[=Factors]{Factors()}}, \code{\link[=Design]{Design()}}, \code{\link[=Preset]{Preset()}}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_Update.R
\name{Update}
\alias{Update}
\title{Updates an old object of class \code{Slick} to new S4 class \code{Slick}}
\usage{
Update(slick)
}
\arguments{
\item{slick}{An S3 object of class \code{Slick}}
}
\value{
An S4 object of class \code{Slick}
}
\description{
Updates an old object of class \code{Slick} to new S4 class \code{Slick}
}
\examples{
slick <- Slick() # a dummy old Slick object
slick <- Update(slick) # updated to S4 object

}

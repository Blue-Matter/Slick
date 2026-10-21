#' Western Atlantic Skipjack Tuna MSE Results
#'
#' A `Slick` data object containing MSE results for the western Atlantic
#' skipjack tuna (*Katsuwonus pelamis*) used as a case study in Hordyk
#' et al. (in review). The object is derived from the WSKJ case study
#' available in the Slick App, with Boxplot, Quilt, Spider, and Tradeoff
#' slots populated from the Timeseries data to demonstrate all six plot
#' types supported by the package. Performance indicator values for the
#' additional plot types are summarized as means over the projection period
#' (years 2026--2055).
#'
#' @format An object of class \linkS4class{Slick} with all six plot slots
#' populated: Timeseries, Kobe, Boxplot, Quilt, Spider, and Tradeoff.
#'
#' @source Downloaded via [download_casestudy()] and extended for
#' illustrative purposes.
#'
#' @seealso [download_casestudy()], [App()]
#'
#' @examples
#' # Summary of the object
#' WSKJ
#'
#' # Check the object is valid
#' Check(WSKJ)
#'
#' # Metadata, Management Procedures, and Operating Models
#' Title(WSKJ)
#' Metadata(MPs(WSKJ))
#' Factors(OMs(WSKJ))
#' Design(OMs(WSKJ))
#'
#' # Plots
#' plotTimeseries(WSKJ)
#' plotBoxplot(WSKJ)
#' plotKobe(WSKJ)
#' plotQuilt(WSKJ)
#' plotSpider(WSKJ)
#' plotTradeoff(WSKJ)
#'
#' @examplesIf interactive()
#' # Explore the results in the Slick App
#' App(slick=WSKJ)
"WSKJ"


#  slick <- readRDS('inst/shiny_apps/Slick/data/case_studies/WSKJ.slick')
# #
# # obj$Perf$Det$Labels
# obj$Perf$Det$Codes
# obj$Perf$Det$Description
#
# obj$MP$Labels |> length()
# obj$Perf$Det$Labels |> length()
# obj$Perf$Det$Values |> dim()
#
# obj$Perf$Det$RefNames
# obj$Perf$Det$RefPoints
#
# obj$OM$Factor_Labels
#
# obj$OM$Codes

 split_by_semicolon <- function(string) {
   if (grepl(';',string)) {
     string <- strsplit(string, ';')[[1]]
     string <- gsub("\\.", "", string)
     string <- gsub('[[:digit:]]+', '', string)
     return(trimws(string))
   }
   trimws(string)
 }

#' Convert an object of class `Slick` to class `SlickData`
#'
#' Previously objects loaded into Slick were class `Slick`. New class for these
#' objects is `SlickData`. This function converts the old class to the new class.
#'
#' @param slick An object of class `Slick`
#'
#' @return An object of class `SlickData`
#' @export
#'
Slick2SlickData <- function(slick) {

  out <- SlickData()
  Title(out) <- slick$Text$Title
  Subtitle(out) <- slick$Text$Sub_title
  # Fishery(out) <- slick$Misc$Fishery
  Introduction(out) <- slick$Text$Introduction
  Date(out) <- slick$Misc$Date
  Author(out) <- split_by_semicolon(slick$Misc$Author)
  Email(out) <- split_by_semicolon(slick$Misc$Contact)
  Institution(out) <- split_by_semicolon(slick$Misc$Institution)

  # OMs
  oms <- OMs()
  Design(oms) <- data.frame(slick$OM$Design)
  colnames(Design(oms)) <- slick$OM$Factor_Labels

  for (i in 1:ncol(Design(oms))) {
    Design(oms)[,i] <- slick$OM$Codes[[i]][Design(oms)[,i]]
  }

  Description(oms) <-  slick$OM$Description
  Label(oms) <- slick$OM$Labels

  defaults <- slick$OM$Defaults
  if (!is.null(defaults)) {
    temp_default <- array(1, dim=dim(  Design(oms)))
    for (i in 1:length(defaults)) {
      slick$OM$Codes[[i]][defaults[[i]]]
      temp_default[,i] <- Design(oms)[,i] %in% slick$OM$Codes[[i]][defaults[[i]]]
    }
    Default(oms) <- apply(temp_default, 1, prod) |> as.logical() |> which()
  }

  OMs(out) <- oms

  # MPs
  mps <- MPs(Label=slick$MP$Labels,
             Description=slick$MP$Description)
  MPs(out) <- mps

  # Quilt
  Quilt(out) <- Quilt(slick$Perf$Det$Codes,
                      slick$Perf$Det$Description,
                      slick$Perf$Det$Values)

  # Spider
  Spider(out) <- Spider(slick$Perf$Det$Codes,
                      slick$Perf$Det$Description,
                      slick$Perf$Det$Values)

  # Boxplot
  Boxplot(out) <- Boxplot(slick$Perf$Stoch$Codes,
                      slick$Perf$Stoch$Description,
                      slick$Perf$Stoch$Values)

  # Kobe
  Kobe(out) <- Kobe(slick$Perf$Proj$Codes,
                    slick$Perf$Proj$Description,
                    slick$Perf$Proj$Times,
                    slick$Perf$Proj$Time_lab,
                    slick$Perf$Proj$Values)

  # TimeSeries
  Timeseries(out) <- Timeseries(Label=slick$StateVar$Labels,
                                Description=slick$StateVar$Description,
                                Time=slick$StateVar$Times,
                                TimeNow=slick$StateVar$TimeNow,
                                TimeLab=slick$StateVar$Time_lab,
                                Value=slick$StateVar$Values,
                                RefPoints=slick$StateVar$RefPoints,
                                RefNames=slick$StateVar$RefNames
                                )


  out
}

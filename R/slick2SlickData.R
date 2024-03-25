 slick <- readRDS('inst/shiny_apps/Slick/data/case_studies/WSKJ.slick')
#
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


Slick2SlickData <- function(slick) {

  out <- SlickData()
  Title(out) <- slick$Text$Title
  Subtitle(out) <- slick$Text$Sub_title
  Fishery(out) <- slick$Misc$Fishery
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
  OMs(out) <- oms

  # MPs
  mps <- newMPs()
  Label(mps) <- slick$MP$Labels
  Description(mps) <- slick$MP$Description
  MPs(out) <- mps

  # Quilt
  quilt <- newQuilt()
  Label(quilt) <- slick$Perf$Det$Codes
  Description(quilt)
  slotNames(quilt)

  # Spider

  # Boxplot

  # Kobe

  # TimeSeries


  out
}

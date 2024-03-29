#  slick <- readRDS('inst/shiny_apps/Slick/data/case_studies/WSKJ.slick')


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
  Introduction(out) <- slick$Text$Introduction
  Date(out) <- slick$Misc$Date
  Author(out) <- slick$Misc$Author
  Email(out) <- slick$Misc$Contact
  Institution(out) <- slick$Misc$Institution

  # OMs
  oms <- OMs()

  # make data.frame
  df_list <- list()
  for (i in seq_along(slick$OM$Codes)) {
    df_list[[i]] <- data.frame(Factor=slick$OM$Factor_Labels[i],
                               Level=slick$OM$Codes[[i]],
                               Description=slick$OM$Description[[i]],
                               Default=FALSE)
    if (!is.null(slick$OM$Defaults)) {
      df_list[[i]]$Default[slick$OM$Defaults[[i]]] <- TRUE
    }
  }
  Metadata(oms) <- do.call('rbind', df_list)

  Design(oms) <-slick$OM$Design
  colnames(Design(oms)) <- slick$OM$Factor_Labels

  for (i in 1:ncol(Design(oms))) {
    Design(oms)[,i] <- slick$OM$Codes[[i]][Design(oms)[,i]]
  }

  OMs(out) <- oms


  # MPs
  MPs(out) <- data.frame(Code=slick$MP$Codes,
                    Label=slick$MP$Labels,
                    Description=slick$MP$Description,
                    Link=NA,
                    Color=slick$Misc$Cols$MP,
                    Default=FALSE)

  # Quilt
  quilt <- Quilt()
  Metadata(quilt) <- data.frame(Code=slick$Perf$Det$Codes,
                                Label=slick$Perf$Det$Labels,
                                Description=slick$Perf$Det$Description,
                                Default=FALSE,
                                MinValue=0,
                                MaxValue=1)

  Value(quilt) <- slick$Perf$Det$Values


  stop(' UP TO HERE')

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

#  slick <- readRDS('inst/shiny_apps/Slick/data/case_studies/WSKJ.slick')


#' Updates an old object of class `Slick` to  new S4 class `Slick`
#'
#'
#' @param slick An object of class `Slick`
#'
#' @return An object of class `Slick`
#' @export
#'
Update <- function(slick) {

  out <- Slick()
  Title(out) <- slick$Text$Title
  Subtitle(out) <- slick$Text$Sub_title

  Date(out) <- slick$Misc$Date
  Author(out) <- slick$Misc$Author
  Email(out) <- slick$Misc$Contact
  Institution(out) <- slick$Misc$Institution

  Introduction(out) <- slick$Text$Introduction

  # MPs
  MPs(out) <- data.frame(Code=slick$MP$Codes,
                         Label=slick$MP$Labels,
                         Description=slick$MP$Description,
                         Color=slick$Misc$Cols$MP)

  # OMs
  oms <- OMs()

  df_list <- list()
  for (i in seq_along(slick$OM$Codes)) {
    df_list[[i]] <- data.frame(Factor=slick$OM$Factor_Labels[i],
                               Level=slick$OM$Codes[[i]],
                               Description=slick$OM$Description[[i]],
                               Set=NA)
    if (!is.null(slick$OM$Default)) {
      df_list[[i]]$Set[slick$OM$Defaults[[i]]] <- 'Default'
    }
  }
  Metadata(oms) <- do.call('rbind', df_list)

  Design(oms) <-slick$OM$Design
  colnames(Design(oms)) <- slick$OM$Factor_Labels

  for (i in 1:ncol(Design(oms))) {
    Design(oms)[,i] <- slick$OM$Codes[[i]][Design(oms)[,i]]
  }

  OMs(out) <- oms




  # Quilt
  quilt <- Quilt()
  Metadata(quilt) <- data.frame(Code=slick$Perf$Det$Codes,
                                Label=slick$Perf$Det$Labels,
                                Description=slick$Perf$Det$Description,
                                Default=FALSE,
                                MinValue=0,
                                MaxValue=1)

  Value(quilt) <- slick$Perf$Det$Values

  MPs(out) <- data.frame(Code=slick$MP$Codes,
                         Label=slick$MP$Labels,
                         Description=slick$MP$Description,
                         Link=NA,
                         Color=slick$Misc$Cols$MP,
                         Default=FALSE)


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

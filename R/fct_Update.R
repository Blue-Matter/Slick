# example_OMs_metadata <- data.frame(Factor=c(rep('Example.1', 2),
#                                             rep('Example.2', 3)),
#                                    Level=c(0.1,0.2, 10, 20, 30),
#                                    Description=c('Description of Example.1 Level 1',
#                                                  'Description of Example.1 Level 2',
#                                                  'Description of Example.2 Level 1',
#                                                  'Description of Example.2 Level 2',
#                                                  'Description of Example.2 Level 2'),
#                                    Default=c(TRUE, TRUE, TRUE, TRUE, FALSE)
# )
#
# example_OMs_design <- data.frame(Example1=c(0.1,0.2),
#                                  Example.2=c(rep(10,2), rep(20,2), rep(30,2))
# )




#  slick <- readRDS('inst/shiny_apps/Slick/data/case_studies/WSKJ.slick')


#' Updates an old object of class `Slick` to new S4 class `Slick`
#'
#'
#' @param slick An S3 object of class `Slick`
#'
#' @return An S4 object of class `Slick`
#' @export
#'
Update <- function(slick) {
  if (isS4(slick))
    return(slick)

  slick_in <- slick
  slick <- Slick()
  Title(slick) <- slick_in$Text$Title
  Subtitle(slick) <- slick_in$Text$Sub_title

  Date(slick) <- slick_in$Misc$Date
  Author(slick) <- slick_in$Misc$Author
  Email(slick) <- slick_in$Misc$Contact
  Institution(slick) <- slick_in$Misc$Institution

  intro <- slick_in$Text$Introduction
  if (inherits(intro, 'list')) {
    intro <- do.call('rbind', intro)
    intro <- paste(intro, collapse = '\n\n')
  }
  Introduction(slick) <- intro

  # MPs
  MPs(slick) <- MPs(Metadata=data.frame(Code=slick_in$MP$Codes,
                                      Label=slick_in$MP$Labels,
                                      Description=slick_in$MP$Description,
                                      Color=slick_in$Misc$Cols$MP))

  # OMs
  oms <- OMs()

  df_list <- list()
  for (i in seq_along(slick_in$OM$Codes)) {
    df_list[[i]] <- data.frame(Factor=slick_in$OM$Factor_Labels[i],
                               Level=slick_in$OM$Codes[[i]],
                               Description=slick_in$OM$Description[[i]])
  }

  Metadata(oms) <- do.call('rbind', df_list)

  Design(oms) <-slick_in$OM$Design
  colnames(Design(oms)) <- slick_in$OM$Factor_Labels

  for (i in 1:ncol(Design(oms))) {
    Design(oms)[,i] <- slick_in$OM$Codes[[i]][Design(oms)[,i]]
  }

  if (!is.null(slick_in$OM$Defaults)) {
    Preset(oms) <- list(Default=slick_in$OM$Defaults)
  }

  OMs(slick) <- oms

  # Quilt
  Quilt(slick) <- Quilt(data.frame(Code=slick_in$Perf$Det$Codes,
                                 Label=slick_in$Perf$Det$Labels,
                                 Description=slick_in$Perf$Det$Description,
                                 MinValue=0,
                                 MaxValue=1),
                        Value=slick_in$Perf$Det$Values,
                        Tradeoff=list(slick_in$Perf$Det$Codes[1], slick_in$Perf$Det$Codes[2]))

  # Spider
  Spider(slick) <- Spider(data.frame(Code=slick_in$Perf$Det$Codes,
                                   Label=slick_in$Perf$Det$Labels,
                                   Description=slick_in$Perf$Det$Description,
                                   MinValue=0,
                                   MaxValue=1),
                        slick_in$Perf$Det$Values)

  # Boxplot
  Boxplot(slick) <- Boxplot(data.frame(Code=slick_in$Perf$Stoch$Codes,
                                     Label=slick_in$Perf$Stoch$Labels,
                                     Description=slick_in$Perf$Stoch$Description),

                          slick_in$Perf$Stoch$Values)
  # Kobe
  Kobe(slick) <- Kobe(Metadata=data.frame(Code=slick_in$Perf$Proj$Codes,
                               Label=slick_in$Perf$Proj$Labels,
                               Description=slick_in$Perf$Proj$Description),
                    Time=slick_in$Perf$Proj$Times,
                    TimeLab=slick_in$Perf$Proj$Time_lab,
                    Value=slick_in$Perf$Proj$Values,
                    RefPoints=slick_in$Perf$Proj$RefPoints,
                    RefName=slick_in$Perf$Proj$RefNames)

  # TimeSeries
  Timeseries(slick) <- Timeseries(Metadata=data.frame(Code=slick_in$StateVar$Codes,
                                                    Label=slick_in$StateVar$Labels,
                                                    Description=slick_in$StateVar$Description),
                                Time=slick_in$StateVar$Times,
                                TimeNow=slick_in$StateVar$TimeNow,
                                TimeLab=slick_in$StateVar$Time_lab,
                                Value=slick_in$StateVar$Values,
                                RefPoints=slick_in$StateVar$RefPoints,
                                RefName=slick_in$StateVar$RefNames)


  slick
}

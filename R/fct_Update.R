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




#  slick <- obj <-  readRDS('inst/NSWO.rda')
# slick <- Update(readRDS('inst/NSWO.rda'))
# slick <- Update( readRDS('inst/WSKJ.rda'))
# slick <- Update( readRDS('inst/shiny_apps/Slick/data/case_studies/SLICKobj.rda'))
# slick <- Update( readRDS('inst/shiny_apps/Slick/data/case_studies/NSWO.slick'))
# slick <- readRDS('inst/shiny_apps/Slick/data/case_studies/NSWO.slick')

# slick <- Update(readRDS('inst/NSWO.rda'))
# Defaults(Boxplot(slick))[[1]] <- 'byom'
# Defaults(Boxplot(slick))[[2]] <- 'both'
#
# saveRDS(slick, 'C:/users/user/downloads/slick.slick')

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
  Email(slick) <- ifelse(is.na(slick_in$Misc$Contact), '', slick_in$Misc$Contact)
  Institution(slick) <- slick_in$Misc$Institution

  intro <- slick_in$Text$Introduction
  if (inherits(intro, 'list')) {
    intro <- do.call('rbind', intro)
    intro <- paste(intro, collapse = '\n\n')
  }
  Introduction(slick) <- intro

  # MPs
  MPs(slick) <- MPs(Code=slick_in$MP$Codes,
                    Label=slick_in$MP$Labels,
                    Description=slick_in$MP$Description,
                    Color=slick_in$Misc$Cols$MP)

  # check colors
  ncol <- length(MPs(slick)@Color)
  nMPs <- length(MPs(slick)@Code)
  if (ncol<nMPs) {
    MPs(slick)@Color <- default_mp_colors(nMPs)
  }

  # OMs
  oms <- OMs()
  df_list <- list()
  for (i in seq_along(slick_in$OM$Codes)) {
    df_list[[i]] <- data.frame(Factor=slick_in$OM$Factor_Labels[i],
                               Level=slick_in$OM$Codes[[i]],
                               Description=slick_in$OM$Description[[i]])
  }

  oms@Factors <-  do.call('rbind', df_list)
  oms@Design <- as.data.frame(slick_in$OM$Design)
  colnames(Design(oms)) <- unique(oms@Factors$Factor) # slick_in$OM$Factor_Labels

  for (i in 1:ncol(Design(oms))) {
    Design(oms)[,i] <- slick_in$OM$Codes[[i]][Design(oms)[,i]]
  }

  if (!is.null(slick_in$OM$Defaults)) {
    Preset(oms) <- list(Default=slick_in$OM$Defaults)
  }

  OMs(slick) <- oms


  # Boxplot
  Boxplot(slick) <- Boxplot(Code=slick_in$Perf$Stoch$Codes,
                            Label=slick_in$Perf$Stoch$Labels,
                            Description=slick_in$Perf$Stoch$Description,
                            Value=slick_in$Perf$Stoch$Values,
                            Preset=list())

  # Kobe
  # ref points
  targ_ind <- match('Target', slick_in$Perf$Proj$RefNames[[1]])
  if (!is.na(targ_ind)) {
    lens <- unlist(lapply(slick_in$Perf$Proj$RefPoints, length))
    ind <- which(lens>=targ_ind)
    if (length(ind)>0) {
      Target <- rep(NA, length(slick_in$Perf$Proj$RefPoints))
      for (i in ind) {
        Target[i] <- slick_in$Perf$Proj$RefPoints[[i]][targ_ind]
      }
    } else {
      Target <- NULL
    }
  } else {
    Target <- NULL
  }

  limit_ind <- match('Limit', slick_in$Perf$Proj$RefNames[[1]])
  if (!is.na(limit_ind)) {
    Limit <- unlist(lapply(slick_in$Perf$Proj$RefPoints, '[[', limit_ind))
  } else {
    Limit <- NULL
  }

  time_lab <-  slick_in$Perf$Proj$Time_lab
  if (is.null(time_lab)) time_lab <- 'Year'

  Kobe(slick) <- Kobe(Code=slick_in$Perf$Proj$Codes,
                      Label=slick_in$Perf$Proj$Labels,
                      Description=slick_in$Perf$Proj$Description,
                      Time=slick_in$Perf$Proj$Times,
                      TimeLab =time_lab,
                      Value=slick_in$Perf$Proj$Values,
                      Target=Target,
                      Limit=Limit
                      )

  # Quilt
  Quilt(slick) <- Quilt(Code=slick_in$Perf$Det$Codes,
                        Label=slick_in$Perf$Det$Labels,
                        Description=slick_in$Perf$Det$Description,
                        Value=slick_in$Perf$Det$Values,
                        Preset=list(),
                        Color=c('blue', 'white'),
                        MinValue=0,
                        MaxValue=1
                        )

  # Spider
  Spider(slick) <- Spider(Code=slick_in$Perf$Det$Codes,
                          Label=slick_in$Perf$Det$Labels,
                          Description=slick_in$Perf$Det$Description,
                          Value=slick_in$Perf$Det$Values,
                          Preset=list())



  # TimeSeries
  Timeseries(slick) <- Timeseries(Code=slick_in$StateVar$Codes,
                                  Label=slick_in$StateVar$Labels,
                                  Description=slick_in$StateVar$Description,
                                  Time=slick_in$StateVar$Times,
                                  TimeNow=slick_in$StateVar$TimeNow,
                                  TimeLab='Year',
                                  Value=slick_in$StateVar$Values,
                                  Preset=list(),
                                  Target=NULL,
                                  Limit=NULL)



  # Tradeoff
  Tradeoff(slick) <- Tradeoff(Code=slick_in$Perf$Det$Codes,
                              Label=slick_in$Perf$Det$Labels,
                              Description=slick_in$Perf$Det$Description,
                              Value=slick_in$Perf$Det$Values,
                              Preset=list())
  slick
}

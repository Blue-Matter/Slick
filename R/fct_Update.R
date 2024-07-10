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
  if (isS4(slick)) {
    chkKobe <- try(slick@Kobe@Defaults, silent = TRUE)
    if (inherits(chkKobe, 'try-error'))
      slick@Kobe@Defaults <- list()
    return(slick)
  }

  slick_in <- slick

  slick <- Slick()
  Title(slick) <- slick_in$Text$Title
  Subtitle(slick) <- slick_in$Text$Sub_title

  Date(slick) <- slick_in$Misc$Date
  Author(slick) <- slick_in$Misc$Author
  Email(slick) <- ifelse(is.na(slick_in$Misc$Contact), '', slick_in$Misc$Contact)
  Institution(slick) <- ifelse(is.na(slick_in$Misc$Institution), '', slick_in$Misc$Institution)

  intro <- slick_in$Text$Introduction
  if (inherits(intro, 'list')) {
    intro <- do.call('rbind', intro)
    intro <- paste(intro, collapse = '\n\n')
  }
  Introduction(slick) <- intro

  # MPs ----
  slick <- update_MPs(slick_in, slick)

  # OMs ----
  slick <- update_OMs(slick_in, slick)

  # Boxplot ----
  slick <- update_Boxplot(slick_in, slick)

  # Kobe ----
  slick <- update_Kobe(slick_in, slick)

  # Quilt ----
  slick <- update_Quilt(slick_in, slick)

  # Spider ----
  slick <- update_Spider(slick_in, slick)

  # TimeSeries ----
  slick <- update_Timeseries(slick_in, slick)

  # Tradeoff ----
  slick <- update_Tradeoff(slick_in, slick)


  slick
}

is_populated <- function(obj) {
  if(is.list(obj))
    return(length(unlist(obj))>0)
  length(obj)>0
}


update_MPs <- function(slick_in, slick) {
  mps <- MPs()
  mps@Code <- slick_in$MP$Codes
  mps@Label <- slick_in$MP$Labels
  mps@Description <- slick_in$MP$Description
  mps@Color <- slick_in$Misc$Cols$MP

  # check colors
  ncol <- length(mps@Color)
  nMPs <- length(mps@Code)
  if (ncol<nMPs) {
    mps@Color <- default_mp_colors(nMPs)
  }

  MPs(slick) <- mps

  slick
}

update_OMs <- function(slick_in, slick) {
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
  slick
}

update_Boxplot <- function(slick_in, slick) {
  obj <- slick_in$Perf$Stoch
  boxplot <- Boxplot()
  if (is_populated(obj$Codes))
    Code(boxplot) <- obj$Codes

  if (is_populated(obj$Labels))
    Label(boxplot) <- obj$Labels

  if (is_populated(obj$Description))
    Description(boxplot) <- obj$Description

  Value(boxplot) <- obj$Values
  Boxplot(slick) <- boxplot
  slick
}


update_Kobe <- function(slick_in, slick) {
  obj <- slick_in$Perf$Proj

  # ref points
  targ_ind <- match('Target', obj$RefNames[[1]])
  if (!is.na(targ_ind)) {
    lens <- unlist(lapply(obj$RefPoints, length))
    ind <- which(lens>=targ_ind)
    if (length(ind)>0) {
      Target <- rep(NA, length(obj$Codes))
      for (i in ind) {
        Target[i] <- obj$RefPoints[[i]][targ_ind]
      }
    } else {
      Target <- NULL
    }
  } else {
    Target <- NULL
  }

  limit_ind <- match('Limit', obj$RefNames[[2]])
  if (!is.na(limit_ind)) {
    lens <- unlist(lapply(obj$RefPoints, length))
    ind <- which(lens>=targ_ind)
    if (length(ind)>0) {
      Limit <- rep(NA, length(obj$Codes))
      for (i in ind) {
        Limit[i] <- obj$RefPoints[[i]][targ_ind]
      }
    } else {
      Limit <- NULL
    }
  } else {
    Limit <- NULL
  }

  time_lab <- slick_in$Perf$Proj$Time_lab
  if (is.null(time_lab)) time_lab <- 'Year'

  kobe <- Kobe()
  if (is_populated(obj$Codes))
    Code(kobe) <- obj$Codes

  if (is_populated(obj$Labels))
    Label(kobe) <- obj$Labels

  if (is_populated(obj$Description))
    Description(kobe) <- obj$Description

  Time(kobe) <- slick_in$Perf$Proj$Times

  Target(kobe) <- Target
  Limit(kobe) <- Limit
  Value(kobe) <- obj$Values
  Kobe(slick) <- kobe

  slick
}

update_Quilt <- function(slick_in, slick) {
  obj <- slick_in$Perf$Det

  quilt <- Quilt()
  if (is_populated(obj$Codes))
    Code(quilt) <- obj$Codes

  if (is_populated(obj$Labels))
    Label(quilt) <- obj$Labels

  if (is_populated(obj$Description))
    Description(quilt) <- obj$Description

  Value(quilt) <- obj$Values
  Color(quilt) <- c('blue', 'white')
  MinValue(quilt) <- 0
  MaxValue(quilt) <- 1

  Quilt(slick) <- quilt
  slick
}

update_Spider <- function(slick_in, slick) {
  obj <- slick_in$Perf$Det

  spider <- Spider()
  if (is_populated(obj$Codes))
    Code(spider) <- obj$Codes

  if (is_populated(obj$Labels))
    Label(spider) <- obj$Labels

  if (is_populated(obj$Description))
    Description(spider) <- obj$Description

  Value(spider) <- obj$Values

  Spider(slick) <- spider
  slick
}

update_Timeseries <- function(slick_in, slick) {
  obj <- slick_in$StateVar
  timeseries <- Timeseries()

  if (is_populated(obj$Codes))
    Code(timeseries) <- obj$Codes

  if (is_populated(obj$Labels))
    Label(timeseries) <- obj$Labels

  if (is_populated(obj$Description))
    Description(timeseries) <- obj$Description

  Time(timeseries) <-  obj$Times
  TimeNow(timeseries) <- obj$TimeNow
  Value(timeseries) <- obj$Values

  Timeseries(slick) <- timeseries
  slick
}

update_Tradeoff <- function(slick_in, slick) {
  obj <- slick_in$Perf$Det

  tradeoff <- Tradeoff()
  if (is_populated(obj$Codes))
    Code(tradeoff) <- obj$Codes

  if (is_populated(obj$Labels))
    Label(tradeoff) <- obj$Labels

  if (is_populated(obj$Description))
    Description(tradeoff) <- obj$Description

  Value(tradeoff) <- obj$Values

  Tradeoff(slick) <- tradeoff
  slick
}



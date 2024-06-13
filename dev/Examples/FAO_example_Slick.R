# Simple example with three MPs

library(Slick)
library(openMSE)

# ---------------------------------------------------------------------------- #

MSE <- readRDS('dev/Examples/Example_1.mse')

# Performance Metrics -----
# Calculate Performance Metrics
PGK <- function(MSE = NULL, Ref = 1, Yrs = NULL) {
  Yrs <- ChkYrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability of Green Kobe"
  PMobj@Caption <- "Probability of Green Kobe"
  PMobj@Ref <- Ref
  PMobj@Stat <- MSE@SB_SBMSY[, , Yrs[1]:Yrs[2]] > 1 & MSE@F_FMSY[, , Yrs[1]:Yrs[2]]  < 1
  PMobj@Prob <- calcProb(PMobj@Stat, MSE)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(PGK) <- 'PM'

pnLRP <- function(MSE = NULL, Ref = 0.4, Yrs = NULL) {
  Yrs <- ChkYrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability of Not Breaching Limit Reference Point"
  PMobj@Caption <- "Probability of Not Breaching Limit Reference Point"
  PMobj@Ref <- Ref
  PMobj@Stat <- MSE@SB_SBMSY > Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSE)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(pnLRP) <- 'PM'

STY <- function(MSE = NULL, Ref = 1, Yrs = 10) {
  Yrs <- ChkYrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- "Short-Term Yield"
  PMobj@Caption <- "Average Yield in First 10 Years"
  PMobj@Ref <- Ref
  PMobj@Stat <- MSE@Catch[,,Yrs]
  PMobj@Prob <- calcProb(PMobj@Stat, MSE)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(STY) <- 'PM'

LTY <- function(MSE = NULL, Ref = 1, Yrs = c(11,20)) {
  Yrs <- ChkYrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- "Short-Term Yield"
  PMobj@Caption <- "Average Yield in Last 10 Years"
  PMobj@Ref <- Ref
  PMobj@Stat <- MSE@Catch[,,Yrs]
  PMobj@Prob <- calcProb(PMobj@Stat, MSE)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(LTY) <- 'PM'




Stability <- function (MSE = NULL, Ref=0.25, Yrs=NULL)  {

  Yrs <- ChkYrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- 'Stability: Mean Variation in TAC (%) between management cycles'
  PMobj@Caption <- 'Mean Variation in TAC (%)'

  TAC <- MSE@TAC[,,Yrs[1]:Yrs[2], drop=FALSE]

  # get management cycle
  nMPs <- MSE@nMPs
  interval <- rep(2, nMPs)

  yrs <- seq_along(Yrs[1]:Yrs[2])
  change_yrs <- seq(1, by=interval[1], to=max(yrs))
  varC <- array(NA, dim=c(MSE@nsim, length(change_yrs)-1, MSE@nMPs))
  for (mm in 1:nMPs) {
    change_yrs <- seq(1, by=interval[mm], to=max(yrs))
    y1 <- change_yrs[1:(length(change_yrs)-1)]
    y2 <- change_yrs[2:length(change_yrs)]
    mat <- (((TAC[, mm, y2] - TAC[, mm, y1])/TAC[,mm , y1])^2)^0.5
    if (dim(mat)[2] < dim(varC)[2]) {
      dd <- dim(varC)[2] - dim(mat)[2]
      mat <- abind::abind(mat, matrix(NA, nrow=MSE@nsim, ncol=dd))
    }

    if (interval[mm]==Inf) {
      varC[,,mm] <- 0
    } else {
      varC[,,mm] <- mat
    }
  }

  PMobj@Stat <- varC
  PMobj@Ref <- Ref
  PMobj@Prob <- apply(PMobj@Stat<Ref, 2:3, mean, na.rm=TRUE)
  PMobj@Mean <- apply(PMobj@Stat, 3, mean, na.rm=TRUE)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(Stability) <- 'PM'


pgk <- PGK(MSE)
pnlrp <- pnLRP(MSE)
sty <- STY(MSE)
lty <- LTY(MSE)
stab <- Stability(MSE)

df <- data.frame(x=pgk@Mean, y=lty@Mean, MP=pgk@MPs)

library(ggplot2)
library(ggrepel)

df <- df |> dplyr::filter(x>0.60, x<0.7) |>
  dplyr::arrange(y)

ggplot(df, aes(x=x, y=y, color=MP)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label=MP)) +
  expand_limits(x=c(0,1), y=c(0,1)) +
  guides(color='none')



# Select 3 MPs
MSE@MPs |> sort()

MP_select <- c('SPmod', 'Itarget1', 'DAAC')


df <- data.frame(x=pgk@Mean, y=lty@Mean, MP=pgk@MPs)
#df <- data.frame(x=pgk@Mean, y=sty@Mean, MP=pgk@MPs)
df2 <- df |> dplyr::filter(MP%in% MP_select)

ggplot(df2, aes(x=x, y=y, color=MP)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label=MP)) +
  expand_limits(x=c(0,1), y=c(0,1)) +
  guides(color='none')


## Subset MSE ----
MSE <- readRDS('dev/Examples/Example_1.mse')
MSE <- Sub(MSE, MPs=MP_select)

# Make Slick object and plots ----


## Slick object ----
slick <- Slick()

Title(slick) <- 'A simple example with three management procedures'
Author(slick) <- 'Adrian Hordyk'
Email(slick) <- 'adrian@bluematterscience.com'
Institution(slick) <- 'Blue Matter Science'

Introduction(slick) <- 'This is an example Slick object.

It is designed to demonstrate some of the features of Slick. See the [Slick Website](https://slick.bluematterscience.com/) for more information
'

## MPs ----

MPs <- MPs()

Metadata(MPs) <- data.frame(Code=c('MP 1',
                                   'MP 2',
                                   'MP 3'),
                            Label=c('MP 1', 'MP 2', 'MP 3'),
                            Description=c('A model-free method where the TAC is incrementally adjusted based on the estimated trend in surplus production.',
                                          'A model-free method where the TAC is incrementally adjusted (starting from reference level that is a fraction of mean recent catches) to reach a index level.',
                                          'A surplus production stock reduction analysis that uses a demographically derived prior for intrinsic rate of increase.'
                                          ))



MPs(slick) <- MPs


## OMs ----
# TODO


## Tradeoff ----


tradeoff <- Tradeoff(Code=c('PGK',
                            'PnLRP',
                            'STY',
                            'LTY',
                            'AAVY'),
                     Label=c('Prob. Green Kobe',
                             'Prob. Biomass Not Falling Below 0.4BMSY',
                             'Short-Term Yield (t)',
                             'Long-Term Yield (t)',
                             'Average Change in TAC'),
                     Description = c('Probability of occurring in the green quadrant of the Kobe plot (B≥BMSY and F<FMSY) over the 20-year projection period.',
                                     'Probability of Not Breaching Limit Reference Point (0.4BMSY) in in every year of the 20-year projection period',
                                     'Average Short-Term Catch (Years 1-9)',
                                     'Average Long-Term Catch (Years 10-20)',
                                     'Average change in TAC between management cycles'))

Value(tradeoff) <- array(NA, dim=c(1, MSE@nMPs, nrow(Metadata(tradeoff))))

Value(tradeoff)[1,, 1] <- PGK(MSE)@Mean
Value(tradeoff)[1,, 2] <- pnLRP(MSE)@Mean
Value(tradeoff)[1,, 3] <- STY(MSE)@Mean
Value(tradeoff)[1,, 4] <- LTY(MSE)@Mean
Value(tradeoff)[1,, 5] <- Stability(MSE)@Mean

Tradeoff(slick) <- tradeoff

theme <- ggplot2::theme(axis.text=element_text(size=8),
                        axis.title=element_text(size=10),
                        )

p1 <- plotTradeoff(slick, XPM='STY', YPM='LTY', lab_size=3, point_size=1) + theme
p2 <- plotTradeoff(slick, XPM='PGK', YPM='LTY', lab_size=3, point_size=1) + theme
p3 <- plotTradeoff(slick, XPM='PnLRP', YPM='LTY', lab_size=3, point_size=1) + theme
p4 <- plotTradeoff(slick, XPM='AAVY', YPM='LTY', lab_size=3, point_size=1) + theme

trade_plot <- cowplot::plot_grid(p1, p2, p3,p4)
trade_plot
ggsave('dev/Examples/Tradeoff.png', trade_plot, width=7, height=6)


## Kobetime ----

kobe <- Kobe(Code=c('SB/SBMSY', 'F/FMSY'),
             Label=c('SB/SBMSY', 'F/FMSY'),
             Description=c('Spawning biomass relative to SBMSY',
                           'Fishing mortality relative to FMSY'))


Time(kobe) <- seq(2025, by=1, length.out=MSE@proyears)
Value(kobe) <- array(NA, c(MSE@nsim, 1, MSE@nMPs, 2, MSE@proyears))
Value(kobe)[,1,,1,] <- MSE@SB_SBMSY
Value(kobe)[,1,,2,] <- MSE@F_FMSY
Kobe(slick) <- kobe


theme <- ggplot2::theme(axis.text=element_text(size=8),
                        axis.title=element_text(size=10),
                        plot.title = ggplot2::element_text(size=8, face='bold')

)

p1 <- Kobe_time_plot(slick, mp=1, xvar=1, yvar=2) +labs(x='Projection Year') + theme
p2 <- Kobe_time_plot(slick, mp=2, xvar=1, yvar=2) +labs(x='Projection Year') + theme
p3 <- Kobe_time_plot(slick, mp=3, xvar=1, yvar=2)  +labs(x='Projection Year') + theme

p <- cowplot::plot_grid(p1, p2, p3, nrow=1)

p

ggsave('dev/Examples/Kobe_Time.png', p, width=9, height=3)

## Time Series ----
timeseries <- Timeseries(Code=c('SB/MSY', 'Catch'),
                         Label=c('Spawning Biomass relative to SBMSY', 'Catch (t)'),
                         Description = c('Historical and projected spawning biomass relative to spawning biomass at maximum sustainable yield',
                                         'Historial and projected catch')
)

hist_years <- rev(seq(2024, by=-1, length.out=MSE@nyears))
proj_years <- seq(2025, by=1, length.out=MSE@proyears)

Time(timeseries) <- c(hist_years, proj_years)
TimeNow(timeseries) <- 2024
Value(timeseries) <- array(NA, dim=c(MSE@nsim, 1, MSE@nMPs, 2, MSE@nyears+MSE@proyears))

SSB_BMSY_hist <- MSE@SSB_hist/MSE@RefPoint$SSBMSY[,1, 1:MSE@nyears]
SSB_BMSY_hist <- replicate(MSE@nMPs, SSB_BMSY_hist) |> aperm(c(1,3,2))

CB_hist <- replicate(MSE@nMPs, MSE@CB_hist) |> aperm(c(1,3,2))

Value(timeseries)[,1,,1,] <- abind::abind(SSB_BMSY_hist, MSE@SB_SBMSY , along=3)
Value(timeseries)[,1,,2,] <- abind::abind(CB_hist, MSE@TAC , along=3)

Target(timeseries) <- c(1, NA)
Limit(timeseries) <- c(0.4, NA)
Timeseries(slick) <- timeseries

MSE@MPs
apply(MSE@CB_hist, 2, median)
apply(MSE@Catch[,1,], 2, median)
apply(MSE@Catch[,2,], 2, median)
apply(MSE@Catch[,3,], 2, median)

MSE@Catch[,3,1]

p1 <- plotTimeseries(slick, size.axis.title = 8, size.axis.text = 8)
p2 <- plotTimeseries(slick, 2,size.axis.title = 8, size.axis.text = 8)
time_series <- cowplot::plot_grid(p1, p2, nrow=1)

time_series

ggsave('dev/Examples/time_series.png', time_series, width=6.2, height=3)

plotTimeseries(slick, MP_ind=1)
plotTimeseries(slick, MP_ind=2, inc_y_label = FALSE)
plotTimeseries(slick, MP_ind=3, inc_y_label = FALSE)


apply(MSE@TAC, 2:3, median)
MSE@TAC[,1,1]
MSE@TAC

plotTimeseries(slick, 2)
plotTimeseries(slick, 2, MP_ind=1)
plotTimeseries(slick, 2, MP_ind=2, inc_y_label = FALSE)
plotTimeseries(slick, 2, MP_ind=3, inc_y_label = FALSE)

plotTimeseries(slick,2)

plotTimeseries(slick, 1, 2)
plotTimeseries(slick, 1, OM_ind=1)

plotTimeseries(slick, 1, includeHist = FALSE)


AAVY(MSE)


# Make Slick object

## TODO:
# - update show function for slick object and all plotting objects
# - write Check function for slick
# - test Check function in App



Check <- function(slick) {

  error <- 0
  warning <- 0

  add_error <- function(error) {
    error +1
  }
  print_error <- function(error) {
    if (error==0)
      usethis::ui_info('Errors found in Slick object:')
  }

  # title, etc
  if (length(Title(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires a Title; use {usethis::ui_code('Title(slick_object) <- \\' My Title \\' ')}")
    error <- add_error(error)
  }

  if (length(Author(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires at least one Author; use {usethis::ui_code('Author(slick_object) <- \\'Author Name\\' ')}")
    error <- add_error(error)
  }

  if (length(Email(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires at least one Email; use {usethis::ui_code('Email(slick_object) <- \\'Email Address\\' ')}")
    error <- add_error(error)
  }

  if (length(Institution(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires at least one Institution; use {usethis::ui_code('Institution(slick_object) <- \\'Institution\\'')}")
    error <- add_error(error)
  }

  if (length(Introduction(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires Introduction; use {usethis::ui_code('Introduction(slick_object) <- \\'Introduction text ...\\'')}")
    error <- add_error(error)
  }

  mps_valid <- methods::validObject(slick@MPs, test=TRUE)
  nMPs <- nrow(Metadata(slick@MPs))
  if (nMPs<2) {
    print_error(error)
    usethis::ui_oops(glue::glue("Slick object has ", nMPs, " management procedures. Requires at least two MPs. ", "See {usethis::ui_code('MPs(slick_object)')}"))
    error <- add_error(error)
  }


  if (error>0)
    stop(error, ' errors found in Slick object', call.=FALSE)
  invisible(slick)
}







# MPs ----




# OMs ----
?OMs
?OMs()

slick

# Tradeoff ----



# Kobe time plot  ----
Kobe()

# Line plots of Biomass and Catch  ----
Timeseries()


## UP TO HERE
# document OMs properly
# add validate functions for OMs
# populate and check for a single OM - remove from filters
myOMs <- OMs()


saveRDS(slick, 'dev/Examples/Example_1.slick')

Slick()

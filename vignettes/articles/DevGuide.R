
library(Slick)

## ---- load_openMSE ----
library(openMSE)

## ---- create_OMs ----
nsim <- 12
OM_Base <- new('OM', Albacore, Generic_IncE, Generic_Obs, nsim=nsim)
OM_Base@D <- c(0.3,0.4)

OM_LowM <- OM_HighM <- OM_Base
OM_LowM@M <- OM_Base@M * 0.75
OM_HighM@M <- OM_Base@M * 1.3333

## ---- specify_MPs ----

MPs <- c('AvC', 'Itarget1', 'DD', 'SPmod')

## ---- run_MSEs ----

MSE_Base <- runMSE(OM_Base, MPs=MPs, silent = TRUE)
MSE_LowM <- runMSE(OM_LowM, MPs=MPs, silent = TRUE)
MSE_HighM <- runMSE(OM_HighM, MPs=MPs, silent = TRUE)

MSE_LowH <- runMSE(OM_LowM, MPs=MPs, silent = TRUE)
MSE_HighH <- runMSE(OM_HighM, MPs=MPs, silent = TRUE)


## ---- create_slick ----
slick <- Slick()

## ---- assign_date ----
Date(slick)
Date(slick) <- Sys.Date()

## ---- write_metadata ----
Title(slick) <- 'An Example Slick Object'
Subtitle(slick) <- "For Testing and Demonstration Purposes"
Date(slick) <- Sys.Date()
Author(slick) <- 'Adrian Hordyk'
Email(slick) <-  "[adrian@bluematterscience.com](mailto:adrian@bluematterscience.com)"
Institution(slick) <- "[Blue Matter Science](bluematterscience.com)"

## ---- read_metadata ----
Title(slick)
Subtitle(slick)
Date(slick)
Author(slick)
Email(slick)
Institution(slick)

## ---- markdown_intro ----
Introduction(slick) <- "

This *example* Slick Object has been designed to demonstrate the main features of **Slick**.

The code used to create this object is available in the [Slick Developer's Guide](https://slick.bluematterscience.com/articles/DevelopersGuide.html).

The information shown here is for demonstration purposes only.

"
## ---- markdown_intro_read ----
Introduction(slick, markdown = TRUE)

## ---- markdown_intro_read2 ----
shiny::hr()
Introduction(slick, markdown = TRUE)
shiny::hr()

## ---- multi_language_intro_write ----
Title(slick) <- list(en='An Example Slick Object',
                     es='Un ejemplo de un objeto Slick',
                     fr="Un exemple d'objet Slick")

## ---- multi_language_intro_read ----
Title(slick)

Title(slick, 'es')

Title(slick, 'fr')

## ---- multi_author ----
Author(slick) <- c('A. Author', 'B. Author')

## ---- multi_email ----
Email(slick) <- c('[a.author@email.com](mailto:a.author@email.com)',
                  '[b.author@email.com](mailto:b.author@email.com)')


Institution(slick) <- c('A Institution', 'B Institution')


## ---- mps_1 -----
mps <- MPs()
Code(mps) <- c('AvC', 'Itarget1', 'DD', 'SPmod')
Label(mps) <- c('Average Catch',
                'Index Target',
                'Delay Difference',
                'Surplus Production')

Description(mps) <- c('TAC is fixed for all years to the average catch from the historical period',
                      'TAC is iteratively adjusted to reach a target CPUE',
                      'Delay-difference assessment model used to estimate current biomass $(B)$ and $U_{MSY}$, and $\text{TAC}= U_{MSY}B$',
                      'TAC is incrementally adjusted based on the apparent trend in surplus production')

## ---- mps_2 -----
mps <- MPs(Code=c('AvC', 'Itarget1', 'DD', 'SPmod'),
           Label=c('Average Catch',
                   'Index Target',
                   'Delay Difference',
                   'Surplus Production'),
           Description = c('TAC is fixed for all years to the average catch from the historical period',
                           'TAC is iteratively adjusted to reach a target CPUE',
                           'Delay-difference assessment model used to estimate current biomass $(B)$ and $U_{MSY}$, and $\text{TAC}= U_{MSY}B$',
                           'TAC is incrementally adjusted based on the apparent trend in surplus production'))



## ---- mps_metadata ----
Metadata(mps) <- data.frame(
  Code=c('AvC', 'Itarget1', 'DD', 'SPmod'),
  Label=c('Average Catch',
          'Index Target',
          'Delay Difference',
          'Surplus Production'),
  Description=c('TAC is fixed for all years to the average catch from the historical period',
                'TAC is iteratively adjusted to reach a target CPUE',
                'Delay-difference assessment model used to estimate current biomass $(B)$ and $U_{MSY}$, and $\text{TAC}= U_{MSY}B$',
                'TAC is incrementally adjusted based on the apparent trend in surplus production')
)

Metadata(mps)

## ---- mps_color ----
Color(mps)

Color(mps) <- colors()[sample(1:100,4)]
Color(mps)

# reset to default colors
Color(mps) <- default_mp_colors(4)


## ---- mps_preset ----
Preset(mps) <- list('All'=1:4,
                    'Model-Free'=1:2,
                    'Model-Based'=3:4)

## ---- mps_print ----
mps

## ---- mps_add ----
MPs(slick) <- mps

## ---- om_create ----

oms <- OMs()

## ---- om_factors ----
Factors(oms) <- data.frame(Factor='M',
                           Level=c('Base', 'Low M', 'High M'),
                           Description=c('Base Case',
                                         'Lower Natural Mortality',
                                         'Higher Natural Mortality')
)

Factors(oms)

## ---- om_design ----

Design(oms) <- data.frame(M=c('Base', 'Low M', 'High M'))

Design(oms)

## ---- om_design_name

rownames(Design(oms)) <- c('Base Case', 'Less Productive', 'More Productive')

## ---- om_preset ----

Preset(oms) <- list('Base Case'=list(1),
                    'Low M' = list(2),
                    'High M' = list(3),
                    'All'= list(1:3)
)


## ---- om_add ----
OMs(slick) <- oms

## ---- timeseries_create ----
timeseries <- Timeseries()

## ---- timeseries_metadata ----

Metadata(timeseries) <- data.frame(
  Code=c('SB/SBMSY', 'F/FMSY', 'Removals'),
  Label=c('SB/SBMSY', 'F/FMSY', 'Removals'),
  Description=c('Spawning biomass relative to equilibrium spawning biomass corresponding with maximum sustainable yield (MSY)',
                'Fishing mortality relative to F_MSY',
                'Removals (Landings + Discards)')
)

Code(timeseries)

## ---- timeseries_time ----
Time(timeseries) <- c(rev(seq(2024, by=-1, length.out=50)),
                      seq(2025, by=1, length.out=50))

## ---- timeseries_timenow ----
TimeNow(timeseries) <- 2024

## ---- timeseries_timelab ---
TimeLab(timeseries) <- 'Year'


## ---- timeseries_value ----
nOM <- nrow(Design(slick))
nMP <- length(Code(mps))
nPI <- length(Code(timeseries))
nTS <- length(Time(timeseries))

Value(timeseries) <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))

## ---- timeseries_value_calculate ----

# SB/SBMSY
SB_SBMSY_Base <- abind::abind(replicate(nMP, MSE_Base@SSB_hist) |> aperm(c(1,3,2)),
                   MSE_Base@SSB,
                   along=3)/MSE_Base@RefPoint$SSBMSY

SB_SBMSY_LowM <- abind::abind(replicate(nMP, MSE_LowM@SSB_hist) |> aperm(c(1,3,2)),
                               MSE_LowM@SSB,
                              along=3)/MSE_LowM@RefPoint$SSBMSY

SB_SBMSY_HighM <- abind::abind(replicate(nMP, MSE_HighM@SSB_hist) |> aperm(c(1,3,2)),
                               MSE_HighM@SSB,
                               along=3)/MSE_HighM@RefPoint$SSBMSY

# F/FMSY
F_FMSY_Base <- abind::abind(replicate(nMP, MSE_Base@FM_hist) |> aperm(c(1,3,2)),
                            MSE_Base@FM)/MSE_Base@RefPoint$FMSY

F_FMSY_LowM  <- abind::abind(replicate(nMP, MSE_LowM@FM_hist) |> aperm(c(1,3,2)),
                             MSE_LowM@FM)/MSE_LowM@RefPoint$FMSY

F_FMSY_HighM  <- abind::abind(replicate(nMP, MSE_HighM@FM_hist) |> aperm(c(1,3,2)),
                              MSE_HighM@FM)/MSE_HighM@RefPoint$FMSY

# Removals
Removals_Base <- abind::abind(replicate(nMP, apply(MSE_Base@Hist@TSdata$Removals, 1:2, sum))
                              |> aperm(c(1,3,2)),
                              MSE_Base@Removals)

Removals_LowM  <- abind::abind(replicate(nMP, apply(MSE_LowM@Hist@TSdata$Removals, 1:2, sum))
                               |> aperm(c(1,3,2)),
                               MSE_LowM@Removals)

Removals_HighM  <- abind::abind(replicate(nMP, apply(MSE_HighM@Hist@TSdata$Removals, 1:2, sum))
                                |> aperm(c(1,3,2)),
                                MSE_HighM@Removals)



## ---- timeseries_value_populate ----

# SB/SBMSY
Value(timeseries)[,1,,1,] <- SB_SBMSY_Base
Value(timeseries)[,2,,1,] <- SB_SBMSY_LowM
Value(timeseries)[,3,,1,] <- SB_SBMSY_HighM

# F/FMSY
Value(timeseries)[,1,,2,] <- F_FMSY_Base
Value(timeseries)[,2,,2,] <- F_FMSY_LowM
Value(timeseries)[,3,,2,] <- F_FMSY_HighM

# Removals
Value(timeseries)[,1,,3,] <- Removals_Base
Value(timeseries)[,2,,3,] <- Removals_LowM
Value(timeseries)[,3,,3,] <- Removals_HighM

## ---- timeseries_target ----

Target(timeseries) <- c(1, 0.8, NA)
Limit(timeseries) <- c(0.5, 1, NA)

## ---- timeseries_check ----
Check(timeseries)

## ---- timeseries_add ----

Timeseries(slick) <- timeseries


## ---- timeseries_plot1 ----

plotTimeseries(slick) # PI 1

plotTimeseries(slick, 2, # PI 2, no quantiles
               includeQuants = FALSE)

plotTimeseries(slick,3, # PI 3, no quantiles, no MP labels
               includeQuants = FALSE,
               includeLabels = FALSE)

## ---- timeseries_plot2 ----

plotTimeseries(slick, byMP=TRUE)

## ---- timeseries_plot3 ----

plotTimeseries(slick, byOM=TRUE)

## ---- timeseries_plot4 ----

plotTimeseries(slick, byOM=TRUE, byMP=TRUE)

## ---- timeseries_plot5 ----

slick_OM1 <- FilterSlick(slick, OMs=1, plot='Timeseries')

plotTimeseries(slick_OM1, byOM=TRUE)
plotTimeseries(slick_OM1) # same plot

## ---- boxplot_create ----

boxplot <- Boxplot(Code=c('SB/SBSMY 25', 'SB/SBSMY 50', 'TAC 25'),
                   Label=c('SB/SBMSY Year 25', 'SB/SBSMY Year 50', 'TAC Year 25'),
                   Description=c('SB/SBMSY in the 25th projection year',
                                 'SB/SBMSY in the last year',
                                 'TAC in the 25th projection year'))

## ---- boxplot_value ----

nOM <- nrow(Design(slick))
nMP <- length(Code(mps))
nPI <- length(Code(boxplot))

Value(boxplot) <- array(NA, dim=c(nsim, nOM, nMP, nPI))

Value(boxplot)[,1,,1] <- MSE_Base@SB_SBMSY[,,25]
Value(boxplot)[,2,,1] <- MSE_LowM@SB_SBMSY[,,25]
Value(boxplot)[,3,,1] <- MSE_HighM@SB_SBMSY[,,25]

Value(boxplot)[,1,,2] <- MSE_Base@SB_SBMSY[,,50]
Value(boxplot)[,2,,2] <- MSE_LowM@SB_SBMSY[,,50]
Value(boxplot)[,3,,2] <- MSE_HighM@SB_SBMSY[,,50]

Value(boxplot)[,1,,3] <- MSE_Base@TAC[,,25]
Value(boxplot)[,2,,3] <- MSE_LowM@TAC[,,25]
Value(boxplot)[,3,,3] <- MSE_HighM@TAC[,,25]

## ---- boxplot_add ----
Check(boxplot)
Boxplot(slick) <- boxplot

## ---- boxplot_plot1 ----

plotBoxplot(slick)

## ---- boxplot_plot2 ----

plotBoxplot(slick, type='violin')

## ---- boxplot_plot3 ----

plotBoxplot(slick, type='both')

## ---- boxplot_plot4 ----
plotBoxplot(slick, byOM=TRUE)

## ---- boxplot_plot5 ----
plotBoxplot(FilterSlick(slick, MPs=1:3, OMs=1:2, plot='Boxplot'),
            byOM=TRUE)


## ---- kobe_create ----

kobe <- Kobe(Code=c('SB/SBMSY', 'F/FMSY'),
             Label=c('SB/SBMSY', 'F/FMSY'),
             Description = c('Spawning biomass relative to SB_MSY',
                             'Fishing mortality relative to F_MSY')
)

## ---- kobe_time ----

Time(kobe) <- seq(2025, by=1, length.out=OM_Base@proyears)

## ---- kobe_value ----

nOM <- nrow(Design(slick))
nMP <- length(Code(mps))
nPI <- length(Code(kobe))
nTS <- length(Time(kobe))

Value(kobe) <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))

Value(kobe)[,1,,1,] <- MSE_Base@SB_SBMSY
Value(kobe)[,2,,1,] <- MSE_LowM@SB_SBMSY
Value(kobe)[,3,,1,] <- MSE_HighM@SB_SBMSY

Value(kobe)[,1,,2,] <- MSE_Base@F_FMSY
Value(kobe)[,2,,2,] <- MSE_LowM@F_FMSY
Value(kobe)[,3,,2,] <- MSE_HighM@F_FMSY

## ---- kobe_add ----
Check(kobe)
Kobe(slick) <- kobe

## ---- kobe_plot ----

plotKobe(slick)

## ---- kobe_timeplot ----

plotKobe(slick, Time=TRUE)

## ---- quilt_create ----

quilt <- Quilt(Code=c('PGK',
                      'P100',
                      'PNOF',
                      'Mean TAC'),
               Label=c('Prob. Green Kobe',
                       'Prob SB>SBMSY',
                       'Prob. Not Overfishing',
                       'Mean Total Allowable Catch'),
               Description = c('Probability of being in the green region of Kobe plot over the projection period',
                               'Probability spawning biomass is greater than SB_MSY over the projection period',
                               'Probability of not overfishing over the projection period',
                               'Mean Total Allowable Catch over the projection period'))

## ---- quilt_value ----

nPI <- length(Code(quilt))

Value(quilt) <- array(NA, dim=c(nOM, nMP, nPI))

Value(quilt)[1,,1] <- apply(MSE_Base@SB_SBMSY > 1 & MSE_Base@F_FMSY < 1, 2, mean)
Value(quilt)[2,,1] <- apply(MSE_LowM@SB_SBMSY > 1 & MSE_LowM@F_FMSY < 1, 2, mean)
Value(quilt)[3,,1] <- apply(MSE_HighM@SB_SBMSY > 1 & MSE_HighM@F_FMSY < 1, 2, mean)

Value(quilt)[1,,2] <- apply(MSE_Base@SB_SBMSY > 1, 2, mean)
Value(quilt)[2,,2] <- apply(MSE_LowM@SB_SBMSY > 1, 2, mean)
Value(quilt)[3,,2] <- apply(MSE_HighM@SB_SBMSY > 1, 2, mean)

Value(quilt)[1,,3] <- apply(MSE_Base@F_FMSY < 1, 2, mean)
Value(quilt)[2,,3] <- apply(MSE_LowM@F_FMSY < 1, 2, mean)
Value(quilt)[3,,3] <- apply(MSE_HighM@F_FMSY < 1, 2, mean)

Value(quilt)[1,,4] <- apply(MSE_Base@TAC, 2, mean)
Value(quilt)[2,,4] <- apply(MSE_LowM@TAC, 2, mean)
Value(quilt)[3,,4] <- apply(MSE_HighM@TAC, 2, mean)

## ---- quilt_maxvalue ----
MinValue(quilt) <- c(0,0,0,NA)
MaxValue(quilt) <- c(1,1,1,NA)

## ---- quilt_add ----
Check(quilt)

Quilt(slick) <- quilt

## ---- quilt_plot ----
plotQuilt(slick)

## ---- quilt_plot2 ----
plotQuilt(slick, minmax=TRUE)

## ---- quilt_plot3 ----

plotQuilt(slick, shading=FALSE, kable=TRUE)


## ---- spider_create ----

spider <- Spider(Code=c('P100',
                      'P50',
                      'PNOF',
                      'Yield'),
               Label=c('Prob SB>SBMSY',
                       'Prob SB>0.5SBMSY',
                       'Prob. Not Overfishing',
                       'Prob. Mean Yield > 50%'),
               Description = c('Probability spawning biomass is greater than SB_MSY over the projection period',
                               'Probability spawning biomass is greater than 0.5 SB_MSY over the projection period',
                               'Probability of not overfishing over the projection period',
                               'Probability mean yield is greater than 50% of the reference yield'))


## ---- spider_value ----

nPI <- length(Code(spider))

Value(spider) <- array(NA, dim=c(nOM, nMP, nPI))

Value(spider)[1,,1] <- MSEtool::P100(MSE_Base)@Mean
Value(spider)[2,,1] <- MSEtool::P100(MSE_LowM)@Mean
Value(spider)[3,,1] <- MSEtool::P100(MSE_HighM)@Mean

Value(spider)[1,,2] <- MSEtool::P50(MSE_Base)@Mean
Value(spider)[2,,2] <- MSEtool::P50(MSE_LowM)@Mean
Value(spider)[3,,2] <- MSEtool::P50(MSE_HighM)@Mean

Value(spider)[1,,3] <- MSEtool::PNOF(MSE_Base)@Mean
Value(spider)[2,,3] <- MSEtool::PNOF(MSE_LowM)@Mean
Value(spider)[3,,3] <- MSEtool::PNOF(MSE_HighM)@Mean

Value(spider)[1,,4] <- MSEtool::LTY(MSE_Base)@Mean
Value(spider)[2,,4] <- MSEtool::LTY(MSE_LowM)@Mean
Value(spider)[3,,4] <- MSEtool::LTY(MSE_HighM)@Mean

## ---- spider_add ----
Check(spider)

Spider(slick) <- spider

## ---- spider_plot ----

plotSpider(slick)

## ---- spider_plot1 ----
plotSpider(slick, fill=TRUE)

## ---- spider_plot2 ----
plotSpider(slick, byMP=TRUE)

## ---- spider_plot3 ----
plotSpider(slick, byOM=TRUE, incMean = FALSE)


## ---- tradeoff_create ----

tradeoff <- Tradeoff(Code=c('P100',
                        'P50',
                        'PNOF',
                        'Yield'),
                 Label=c('Prob SB>SBMSY',
                         'Prob SB>0.5SBMSY',
                         'Prob. Not Overfishing',
                         'Prob. Mean Yield > 50%'),
                 Description = c('Probability spawning biomass is greater than SB_MSY over the projection period',
                                 'Probability spawning biomass is greater than 0.5 SB_MSY over the projection period',
                                 'Probability of not overfishing over the projection period',
                                 'Probability mean yield is greater than 50% of the reference yield'))



## ---- tradeoff_value ----

nPI <- length(Code(tradeoff))

Value(tradeoff) <- array(NA, dim=c(nOM, nMP, nPI))

Value(tradeoff)[1,,1] <- MSEtool::P100(MSE_Base)@Mean
Value(tradeoff)[2,,1] <- MSEtool::P100(MSE_LowM)@Mean
Value(tradeoff)[3,,1] <- MSEtool::P100(MSE_HighM)@Mean

Value(tradeoff)[1,,2] <- MSEtool::P50(MSE_Base)@Mean
Value(tradeoff)[2,,2] <- MSEtool::P50(MSE_LowM)@Mean
Value(tradeoff)[3,,2] <- MSEtool::P50(MSE_HighM)@Mean

Value(tradeoff)[1,,3] <- MSEtool::PNOF(MSE_Base)@Mean
Value(tradeoff)[2,,3] <- MSEtool::PNOF(MSE_LowM)@Mean
Value(tradeoff)[3,,3] <- MSEtool::PNOF(MSE_HighM)@Mean

Value(tradeoff)[1,,4] <- MSEtool::LTY(MSE_Base)@Mean
Value(tradeoff)[2,,4] <- MSEtool::LTY(MSE_LowM)@Mean
Value(tradeoff)[3,,4] <- MSEtool::LTY(MSE_HighM)@Mean

## ---- tradeoff_add ----
Check(tradeoff)

Tradeoff(slick) <- tradeoff

## ---- tradeoff_plot ----

plotTradeoff(slick)

## ---- tradeoff_plot2 ----

plotTradeoff(slick, 1:2, 3:4)


## ---- slick_library ----

saveRDS(slick, '../SlickLibrary/Slick_Objects/Example.slick')

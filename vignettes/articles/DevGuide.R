## ---- install_slick ----
# install.packages('pak')
pak::pkg_install('blue-matter/Slick')
library(Slick)



## ---- load_openMSE ----
library(openMSE)

## ---- create_OMs ----

OM_Base <- new('OM', Albacore, Generic_IncE, Generic_Obs, nsim=24)

OM_LowM <- OM_HighM <- OM_Base
OM_LowM@M <- OM_Base@M * 0.75
OM_HighM@M <- OM_Base@M * 1.3333

MPs <- c('AvC', 'Itarget1', 'MCD', 'SPmod')

MSE_Base <- runMSE(OM_Base, MPs=MPs, silent = TRUE)
MSE_LowM <- runMSE(OM_LowM, MPs=MPs, silent = TRUE)
MSE_HighM <- runMSE(OM_HighM, MPs=MPs, silent = TRUE)


## ---- create_slick ----
slick <- Slick()

## ---- write_metadata ----
Title(slick) <- 'An Example Slick Object'
Subtitle(slick) <- "This is the subtitle"
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

This is an example Slick object. It has been created for the purposes of demonstrating the key features of Slick.

The introduction can include paragraphs as well as Markdown such as *italics* and **bold** text and [Links](https://slick.bluematterscience.com).

"
## ---- markdown_intro_read ----
Introduction(slick, markdown = TRUE)

## ---- markdown_intro_read2 ----
shiny::hr()
Introduction(slick, markdown = TRUE)
shiny::hr()

## ---- multi_language_intro_write ----
Title(slick) <- list(en='This is the English Title',
                     es='This is the Spanish Title',
                     fr='This is the French Title')

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
Code(mps) <- c('MP1', 'MP2', 'MP3', 'MP4')
Label(mps) <- c('MP 1', 'MP 2', 'MP 3', 'MP 4')
Description(mps) <- c('This is an example MP',
                      'This MP is a second example',
                      'A third example MP',
                      'The final example MP')

## ---- mps_2 -----
mps <- MPs(Code=c('MP1',
                  'MP2',
                  'MP3',
                  'MP4'),
           Label=c('MP 1',
                   'MP 2',
                   'MP 3',
                   'MP 4'),
           Description = c('This is an example MP',
                           'This MP is a second example',
                           'A third example MP',
                           'The final example MP'))



## ---- mps_metadata ----
Metadata(mps) <- data.frame(
  Code=c('MP1',
         'MP2',
         'MP3',
         'MP4'),
  Label=c('MP 1',
          'MP 2',
          'MP 3',
          'MP 4'),
  Description=c('This is an example MP',
                'This MP is a second example',
                'A third example MP',
                'The final example MP')
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
                    'First 2'=1:2,
                    'Last 2'=3:4)

## ---- mps_print ----
mps

## ---- mps_add ----
MPs(slick) <- mps

## ---- om_create ----

oms <- OMs()

## ---- om_factors ----
Factors(oms) <- data.frame(Factor=c(rep('M',3), rep('h',2)),
                           Level=c(0.1,0.2,0.3,0.7,0.9),
                           Description=c('Natural Mortality = 0.1',
                                         'Natural Mortality = 0.2',
                                         'Natural Mortality = 0.3',
                                         'Steepness = 0.7',
                                         'Steepness = 0.9')
)

Factors(oms)

## ---- om_design ----

Design(oms) <- data.frame(M=c(0.1, 0.1, 0.2, 0.2, 0.3,0.3),
                          h=rep(c(0.7, 0.9), 3)
)
Design(oms)

## ---- om_design_name

rownames(Design(oms)) <- letters[1:6]

## ---- om_preset ----

Preset(oms) <- list('Low h'=list(1:3, 1),
                    'High h'=list(1:3, 2))

## ---- om_check ----
Check(oms)

## ---- om_add ----
OMs(slick) <- oms

## ---- timeseries_create ----
timeseries <- Timeseries()

## ---- timeseries_metadata ----

Metadata(timeseries) <- data.frame(
  Code=c('PI 1', 'PI 2', 'PI 3'),
  Label=c('Perf. Ind. 1', 'Perf. Ind. 2', 'Perf. Ind. 3'),
  Description=c('This is the description of PI 1',
                'This is the description of PI 2',
                'This is the description of PI 3')
  )

Code(timeseries)

## ---- timeseries_time ----
Time(timeseries) <- 1980:2040

## ---- timeseries_timenow ----
TimeNow(timeseries) <- 2024

## ---- timeseries_timelab ---
TimeLab(timeseries) <- 'Year'



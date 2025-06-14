% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_plotKobe.R
\name{plotKobe}
\alias{plotKobe}
\title{Plot \code{Kobe}}
\usage{
plotKobe(
  slick,
  xPI = 1,
  yPI = 2,
  Time = FALSE,
  BLcol = "#F8DC7A",
  TLcol = "#D8775D",
  TRcol = "#FDBD56",
  BRcol = "#67C18B",
  axis_label = "Code",
  percentile = 0.9,
  axis.text.size = 14,
  axis.title.size = 16,
  strip.text.size = 16,
  strip.text.color = "#D6501C",
  incMP_label = TRUE,
  mp.text.size = 7,
  mp.point.size = 4,
  mp.init.point.size = 2,
  xmax = 2,
  ymax = 2,
  hist_traj = FALSE,
  ncol = 4,
  lang = "en",
  MP_label = "Code"
)
}
\arguments{
\item{slick}{A \code{\link[=Slick-class]{Slick-class()}} object}

\item{xPI}{Numeric value specifying the performance indicator for the x-axis}

\item{yPI}{Numeric value specifying the performance indicator for the y-axis}

\item{Time}{Logical. Kobe Time plot?}

\item{BLcol}{Color for the bottom left quadrant}

\item{TLcol}{Color for the top left quadrant}

\item{TRcol}{Color for the top  right quadrant}

\item{BRcol}{Color for the bottom right quadrant}

\item{axis_label}{Label to use for the axes. Either \code{Code} or \code{Label}.
\code{Description} works as well, but you probably don't want to do that.}

\item{percentile}{Numeric value specifying the percentile for the x and y percentile bars.
Use NULL to remove percentile lines.}

\item{axis.text.size}{Font size for axis text}

\item{axis.title.size}{Font size for axis title}

\item{strip.text.size}{Font size for facet strip text}

\item{strip.text.color}{Color for facet strip text}

\item{incMP_label}{Logical. Include MP labels?}

\item{mp.text.size}{Font size for MP labels}

\item{mp.point.size}{Point size for MP labels}

\item{mp.init.point.size}{Point size for start of trajectory. If \code{hist_traj==TRUE}}

\item{xmax}{Maximum value for the x-axis. Values greater than \code{xmax} will be shown at \code{xmax}}

\item{ymax}{Maximum value for the yx-axis. Values greater than \code{ymax} will be shown at \code{ymax}}

\item{hist_traj}{Logical. Plot the historical trajectories?}

\item{ncol}{Numeric. Number of columns for Kobe Time}

\item{lang}{Optional. Language (if supported in Slick Object). Either 'en', 'es', 'fr'}

\item{MP_label}{Label to use for the MPs. Either \code{Code} or \code{Label}.
\code{Description} works as well, but you probably don't want to do that.}
}
\value{
A \code{ggplot2} object
}
\description{
Plots a Kobe plot for a given projection year, or a Kobe Time plot.
}
\details{
By default \code{plotKobe} shows the terminal projection year.
\code{TimeTerminal(Kobe)} can be used to override this. Use a numeric value indicating the time
(must match a value in \code{Time(Kobe)}) to use for the \code{Kobe} plot.
}
\examples{
# Generate dummy values
nsim <- 10
nOM <- 2
nMP <- 4
nPI <- 2
nTS <- 30

values <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))

pi_means <- c(1,1)

for (om in 1:nOM) {
  for (mp in 1:nMP) {
    for (pi in 1:nPI) {
      values[,om, mp, pi,] <- pi_means[pi] *
        matrix(
        cumprod(c(rlnorm(nTS*nsim, 0, 0.05))),
        nrow=nsim)
    }
  }
}

# Create and populate Object
kobe <- Kobe(Code=c('B/BMSY', 'F/FMSY'),
             Label=c('B/BMSY',
                     'F/FMSY'),
             Description = c('This is the description for PI 1',
                             'This is the description for PI 2'),
             Value=values
)

# Add values for projection time steps
Time(kobe) <- seq(2025, by=1, length.out=nTS)

# Check
Check(kobe)

# Add to `Slick` object
slick <- Slick()
Kobe(slick) <- kobe

# Plots
plotKobe(slick)

plotKobe(slick, Time=TRUE)

}
\seealso{
\code{\link[=Kobe]{Kobe()}}, \code{\link[=Kobe-class]{Kobe-class()}}
}

# Plot `Kobe`

Plots a Kobe plot for a given projection year, or a Kobe Time plot.

## Usage

``` r
plotKobe(
  slick,
  xPI = 1,
  yPI = 2,
  Time = FALSE,
  OMs = NA,
  BLcol = "#F8DC7A",
  TLcol = "#D8775D",
  TRcol = "#FDBD56",
  BRcol = "#67C18B",
  axis_label = "Code",
  percentile = 0.75,
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
```

## Arguments

- slick:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- xPI:

  Numeric value specifying the performance indicator for the x-axis

- yPI:

  Numeric value specifying the performance indicator for the y-axis

- Time:

  Logical. Kobe Time plot?

- OMs:

  Integers representing the OMs to include in the plot. Defaults to all.

- BLcol:

  Color for the bottom left quadrant

- TLcol:

  Color for the top left quadrant

- TRcol:

  Color for the top right quadrant

- BRcol:

  Color for the bottom right quadrant

- axis_label:

  Label to use for the axes. Either `Code` or `Label`. `Description`
  works as well, but you probably don't want to do that.

- percentile:

  Numeric value specifying the percentile for the x and y percentile
  bars. Use NULL to remove percentile lines.

- axis.text.size:

  Font size for axis text

- axis.title.size:

  Font size for axis title

- strip.text.size:

  Font size for facet strip text

- strip.text.color:

  Color for facet strip text

- incMP_label:

  Logical. Include MP labels?

- mp.text.size:

  Font size for MP labels

- mp.point.size:

  Point size for MP labels

- mp.init.point.size:

  Point size for start of trajectory. If `hist_traj==TRUE`

- xmax:

  Maximum value for the x-axis. Values greater than `xmax` will be shown
  at `xmax`

- ymax:

  Maximum value for the yx-axis. Values greater than `ymax` will be
  shown at `ymax`

- hist_traj:

  Logical. Plot the historical trajectories?

- ncol:

  Numeric. Number of columns for Kobe Time

- lang:

  Optional. Language (if supported in Slick Object). Either 'en', 'es',
  'fr', or 'pt'

- MP_label:

  Label to use for the MPs. Either `Code` or `Label`. `Description`
  works as well, but you probably don't want to do that.

## Value

A `ggplot2` object

## Details

By default `plotKobe` shows the terminal projection year.
`TimeTerminal(Kobe)` can be used to override this. Use a numeric value
indicating the time (must match a value in `Time(Kobe)`) to use for the
`Kobe` plot.

## See also

[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md),
[`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)

## Examples

``` r
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
#> 
#> ── Checking: "Kobe" ──
#> 
#> ✔ Complete

# Add to `Slick` object
slick <- Slick()
Kobe(slick) <- kobe

# Plots
plotKobe(slick)
#> Error in plotKobe(slick): object 'nMP' not found

plotKobe(slick, Time=TRUE)
#> Error in plotKobe(slick, Time = TRUE): object 'nMP' not found
```

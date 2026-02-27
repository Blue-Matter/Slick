# S4 class Kobe

Slots can be accessed and assigned using functions corresponding to slot
name. See `See Also` section below.

## Details

Objects of class `Kobe` are created with
[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md)

### Performance Indicators

By default, the first PI will be on the x-axis (usually B/BMSY or
something similar) and the second on the y-axis (e.g., F/FMSY). The
Slick [`App()`](https://slick.bluematterscience.com/reference/App.md)
provides drop down menus for selecting other PIs.

### Multi-Language Support

Text with multi-language supported can be provided as a named list.
Available languages:

- `en`: English (default)

- `es`: Spanish

- `fr`: French

- `pt`: Portuguese

### Note

Character strings in `Code`, `Label`, and `Description` must all be same
length as the number of performance indicators (`nPIs`) in `Value`

## Slots

- `Code`:

  A *short* code for the Performance Indicators for this object. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- `Label`:

  A short label for the Performance Indicators for this object. Used to
  label axes on charts. Can be longer than `Code` but recommended to
  keep short as possible so it shows clearly in plots and tables. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- `Description`:

  A description for the Performance Indicators for this object. Can
  include Markdown, see `Examples`. A character string length `nPI` or a
  named list for multi-language support. See `Details`

- `Time`:

  A numeric vector with values for the projection time-steps. Must match
  length `nTS` in `Value`

- `TimeLab`:

  Character string length 1. Name of the time step (e.g., 'Year'). Will
  be used as the label in the `Kobe Time` plot. Use a named list for
  multiple languages.

- `Value`:

  A numeric array with the stochastic performance indicator values for
  each simulation (sim), operating model (OM), management procedure
  (MP), performance indicator (PI), and projection time-steps (nTS)
  Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)

- `Preset`:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- `Target`:

  Numeric vector length `nPI` with the target value for the PIs. Defines
  the color quadrants on the Kobe plot. Defaults to 1.

- `Limit`:

  Numeric vector length `nPI` with the limit value for the two PIs.
  Shows as red line on Kobe plot. NULL to ignore.

- `Defaults`:

  A list object with default selections for the Kobe See
  [`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md)

- `TimeTerminal`:

  Optional. By default the `Kobe` plot shows the terminal projection
  year. `TimeTerminal` can be used to override this. Use a numeric value
  indicating the time (must match a value in `Time`) to use for the
  `Kobe` plot

- `Misc`:

  A named list for additional miscellaneous information.

## See also

[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md),
[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)

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

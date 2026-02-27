# Methods for Creating, Accessing and Assigning `Kobe` objects

The `Kobe` function is used both to create and modify an
[`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
object. and to access and assign `Kobe` for an object of class
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md).
See `Details`.

## Usage

``` r
Kobe(
  Code = "",
  Label = "",
  Description = "",
  Time = numeric(),
  TimeLab = "Year",
  Value = array(),
  Preset = list(),
  Target = 1,
  Limit = NULL,
  Defaults = list(),
  TimeTerminal = numeric(),
  Misc = list()
)

Kobe(Slick) <- value

# S4 method for class 'missing'
Kobe()

# S4 method for class 'character'
Kobe(
  Code = "",
  Label = "",
  Description = "",
  Time = numeric(),
  TimeLab = "Year",
  Value = array(),
  Preset = list(),
  Target = 1,
  Limit = NULL,
  Defaults = list(),
  TimeTerminal = numeric(),
  Misc = list()
)

# S4 method for class 'list'
Kobe(
  Code = "",
  Label = "",
  Description = "",
  Time = numeric(),
  TimeLab = "Year",
  Value = array(),
  Preset = list(),
  Target = 1,
  Limit = NULL,
  Defaults = list(),
  TimeTerminal = numeric(),
  Misc = list()
)

# S4 method for class 'Slick'
Kobe(Code)

# S4 method for class 'Slick'
Kobe(Slick) <- value
```

## Arguments

- Code:

  A *short* code for the Performance Indicators for this object. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- Label:

  A short label for the Performance Indicators for this object. Used to
  label axes on charts. Can be longer than `Code` but recommended to
  keep short as possible so it shows clearly in plots and tables. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- Description:

  A description for the Performance Indicators for this object. Can
  include Markdown, see `Examples`. A character string length `nPI` or a
  named list for multi-language support. See `Details`

- Time:

  A numeric vector with values for the projection time-steps. Must match
  length `nTS` in `Value`

- TimeLab:

  Character string length 1. Name of the time step (e.g., 'Year'). Will
  be used as the label in the `Kobe Time` plot. Use a named list for
  multiple languages.

- Value:

  A numeric array with the stochastic performance indicator values for
  each simulation (sim), operating model (OM), management procedure
  (MP), performance indicator (PI), and projection time-steps (nTS).
  Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)

- Preset:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- Target:

  Numeric vector length `nPI` with the target value for the PIs. Defines
  the color quadrants on the Kobe plot. Defaults to c(1,1).

- Limit:

  Numeric vector length `nPI` with the limit value for the PIs. Shows as
  red line on Kobe plot. NULL to ignore.

- Defaults:

  A list object with default selections for the Kobe See `Kobe()`

- TimeTerminal:

  Optional. By default the `Kobe` plot shows the terminal projection
  year. `TimeTerminal` can be used to override this. Use a numeric value
  indicating the time (must match a value in `Time`) to use for the
  `Kobe` plot

- Misc:

  A named list for additional miscellaneous information.

- Slick:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- value:

  A
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

## Details

Objects of class `Kobe` are created with `Kobe()`

The Kobe plot typically shows B/BMSY (or something similar) on the
x-axis, and F/FMSY (or something similar) on the y-axis.

### Performance Indicators

The first PI will be on the x-axis (usually B/BMSY or something similar)
and the second on the y-axis (e.g., F/FMSY)

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

## Functions

- `Kobe(missing)`: Create an empty `Kobe` object

- `Kobe(character)`: Create a populated `Kobe` object

- `Kobe(list)`: Create a populated `Kobe` object

- `Kobe(Slick)`: Return `Kobe` from a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `Kobe(Slick) <- value`: Assign a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object to a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

## See also

[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Time()`](https://slick.bluematterscience.com/reference/Time.md),
\[TimeLab(),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md),
[`Target()`](https://slick.bluematterscience.com/reference/Target.md)
and [`Limit()`](https://slick.bluematterscience.com/reference/Target.md)

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

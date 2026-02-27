# Methods for Creating, Accessing and Assigning `Tradeoff` objects

The `Tradeoff` function is used both to create and modify an
[`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
object. and to access and assign `Tradeoff` for an object of class
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md).
See `Details`.

## Usage

``` r
Tradeoff(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Misc = list()
)

Tradeoff(Slick) <- value

# S4 method for class 'missing'
Tradeoff()

# S4 method for class 'character'
Tradeoff(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Misc = list()
)

# S4 method for class 'list'
Tradeoff(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Misc = list()
)

# S4 method for class 'Slick'
Tradeoff(Code)

# S4 method for class 'Slick'
Tradeoff(Slick) <- value
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

- Value:

  A numeric array with the stochastic performance indicator values for
  each operating model (OM), management procedure (MP), and performance
  indicator (PI) Dimensions: c(`nOM`, `nMP`, `nPI`)

- Preset:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- Misc:

  A named list for additional miscellaneous information.

- Slick:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- value:

  A
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

## Details

Objects of class `Tradeoff` are created with `Tradeoff()`

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

- `Tradeoff(missing)`: Create an empty `Tradeoff` object

- `Tradeoff(character)`: Create a populated `Tradeoff` object

- `Tradeoff(list)`: Create a populated `Tradeoff` object

- `Tradeoff(Slick)`: Return `Tradeoff` from a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `Tradeoff(Slick) <- value`: Assign a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object to a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

## See also

[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Metadata()`](https://slick.bluematterscience.com/reference/Metadata.md),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)

## Examples

``` r
# Generate dummy values
nOM <- 2
nMP <- 4
nPI <- 4

values <- array(NA, dim=c(nOM, nMP, nPI))

pi_means <- runif(nPI, 5, 50)
for (om in 1:nOM) {
  for (mp in 1:nMP) {
    for (pi in 1:nPI) {
      values[om, mp, pi] <- rlnorm(1,log(pi_means[pi]), 0.4)
    }
  }
}

# Create and populate Object
tradeoff <- Tradeoff(Code=c('PI1', 'PI2', 'PI3', 'PI4'),
               Label=c('Performance Indicator 1',
                       'Performance Indicator 2',
                       'Performance Indicator 3',
                       'Performance Indicator 4'),
               Description = c('This is the description for PI 1',
                               'This is the description for PI 2',
                               'This is the description for PI 3',
                               'This is the description for PI 4'),
               Value=values)

# Check
Check(tradeoff)
#> 
#> ── Checking: "Tradeoff" ──
#> 
#> ✔ Complete

# Add to `Slick` object
slick <- Slick()
Tradeoff(slick) <- tradeoff

# Plots
plotTradeoff(slick)
#> ℹ Note: `MPs` is empty. Using default MP names and colors


plotTradeoff(slick, c(1,1,2), c(2,3,3))
#> ℹ Note: `MPs` is empty. Using default MP names and colors
```

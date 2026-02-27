# Methods for Creating, Accessing and Assigning `Boxplot` objects

The `Boxplot` function is used both to create and modify an
[`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
object. and to access and assign `Boxplot` for an object of class
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md).
See `Details`.

## Usage

``` r
Boxplot(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Defaults = list("overall", "boxplot"),
  Misc = list()
)

Boxplot(Slick) <- value

# S4 method for class 'missing'
Boxplot()

# S4 method for class 'character_list'
Boxplot(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Defaults = list("overall", "boxplot"),
  Misc = list()
)

# S4 method for class 'Slick'
Boxplot(Code)

# S4 method for class 'Slick'
Boxplot(Slick) <- value
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
  each simulation (sim), operating model (OM), management procedure
  (MP), and performance indicator (PI). Dimensions: c(`nsim`, `nOM`,
  `nMP`, and `nPI`).

- Preset:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- Defaults:

  A list object with default selections for the Boxplot

- Misc:

  A named list for additional miscellaneous information.

- Slick:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- value:

  A
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

## Details

Objects of class `Boxplot` are created with `Boxplot()`

Use
[`plotBoxplot()`](https://slick.bluematterscience.com/reference/plotBoxplot.md)
to create the boxplot from the console.

Use the
[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)
functions to access and assign the values for an existing `Boxplot`
object, see `Examples`

### Multi-Language Support

Text with multi-language supported can be provided as a named list.
Available languages:

- `en`: English (default)

- `es`: Spanish

- `fr`: French

- `pt`: Portuguese

### Note

Character strings in `Code`, `Label`, and `Description` must all be same
length as the number of performance indicators (`nPIs`) in \`Value

### Defaults

`Defaults` is used to select the plot options that are selected in the
Boxplot. It is a list of length 2, with the following requirements for
the list elements:

1.  A character string. Options: 'overall' (default) or 'byom'

2.  A character string. Options: 'boxplot' (default), 'violin', or
    'both'

If unrecognized values are entered, the defaults will be used.

## Functions

- `Boxplot(missing)`: Create an empty `Boxplot` object

- `Boxplot(character_list)`: Create a populated `Boxplot` object

- `Boxplot(Slick)`: Return `Boxplot` from a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `Boxplot(Slick) <- value`: Assign a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object to a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

## See also

[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Metadata()`](https://slick.bluematterscience.com/reference/Metadata.md),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md),
[`Defaults()`](https://slick.bluematterscience.com/reference/Defaults.md),
[`plotBoxplot()`](https://slick.bluematterscience.com/reference/plotBoxplot.md)

## Examples

``` r
# Generate dummy values
nsim <- 10
nOM <- 2
nMP <- 4
nPI <- 3

values <- array(NA, dim=c(nsim, nOM, nMP, nPI))
pi_means <- runif(nPI, 5, 50)
for (om in 1:nOM) {
  for (mp in 1:nMP) {
    for (pi in 1:nPI) {
      values[,om, mp, pi] <- rlnorm(nsim, log(pi_means[pi]), 0.4)
    }
  }
}

# Create and populate Object
boxplot <- Boxplot(Code=c('PI1', 'PI2', 'PI3'),
                   Label=c('Performance Indicator 1',
                           'Performance Indicator 2',
                           'Performance Indicator 3'),
                   Description = c('This is the description for PI 1',
                                   'This is the description for PI 2',
                                   'This is the description for PI 3'),
                   Value=values)

# Check
Check(boxplot)
#> 
#> ── Checking: "Boxplot" ──
#> 
#> ✔ Complete

# Add to `Slick` object
slick <- Slick()
Boxplot(slick) <- boxplot

# Plots
plotBoxplot(slick)
#> ℹ Note: `MPs` is empty. Using default MP names and colors


plotBoxplot(slick, type='violin')
#> ℹ Note: `MPs` is empty. Using default MP names and colors


plotBoxplot(slick, byOM=TRUE)
#> ℹ Note: `MPs` is empty. Using default MP names and colors


plotBoxplot(slick, 2, type='both', byOM=TRUE)
#> ℹ Note: `MPs` is empty. Using default MP names and colors

```

# Methods for Creating, Accessing and Assigning `Spider` objects

The `Spider` function is used both to create and modify an
[`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
object. and to access and assign `Spider` for an object of class
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md).
See `Details`.

## Usage

``` r
Spider(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Misc = list()
)

Spider(Slick) <- value

# S4 method for class 'missing'
Spider()

# S4 method for class 'character'
Spider(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Misc = list()
)

# S4 method for class 'list'
Spider(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Misc = list()
)

# S4 method for class 'Slick'
Spider(Code)

# S4 method for class 'Slick'
Spider(Slick) <- value
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
  indicator (PI). Dimensions: c(`nOM`, `nMP`, and `nPI`). All PI values
  must range between 0 and 1 or 0 and 100. If all values are \<= 1, they
  will be multiplied by 100 in the plot.

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
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

## Details

Objects of class `Spider` are created with `Spider()`

Use the
[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)
functions to access and assign the values for an existing `Spider`
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

## Functions

- `Spider(missing)`: Create an empty `Spider` object

- `Spider(character)`: Create a populated `Spider` object

- `Spider(list)`: Create a populated `Spider` object

- `Spider(Slick)`: Return `Spider` from a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `Spider(Slick) <- value`: Assign a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
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

# Note: PI values must be between 0 and 1, with 1 indicating better performance
pi_means <- runif(nPI, 0, 1)
for (om in 1:nOM) {
  for (mp in 1:nMP) {
    for (pi in 1:nPI) {
      values[om, mp, pi] <- runif(1, pi_means[pi])
    }
  }
}

# Create and populate Object
spider <- Spider(Code=c('PI1', 'PI2', 'PI3', 'PI4'),
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
Check(spider)
#> 
#> ── Checking: "Spider" ──
#> 
#> ✔ Complete

# Add to `Slick` object
slick <- Slick()
Spider(slick) <- spider

# Plots
plotSpider(slick)
#> ℹ Note: `MPs` is empty. Using default MP names and colors


plotSpider(slick, fill=TRUE)
#> ℹ Note: `MPs` is empty. Using default MP names and colors


plotSpider(slick, byMP=TRUE)
#> ℹ Note: `MPs` is empty. Using default MP names and colors


plotSpider(slick, byOM=TRUE)
#> ℹ Note: `MPs` is empty. Using default MP names and colors

```

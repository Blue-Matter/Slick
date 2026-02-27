# S4 class `Spider`

Objects of class `Spider` are used to store information for the Spider
plots. Like all S4 objects in `Slick`, slots in this object can be
accessed and assigned using functions corresponding to slot name. See
[Spider](https://slick.bluematterscience.com/reference/Spider-methods.md)
and the the `See Also` section below.

## Details

Objects of class `Spider` are created with
[`Spider()`](https://slick.bluematterscience.com/reference/Spider-methods.md)

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

- `Value`:

  A numeric array with the stochastic performance indicator values for
  each operating model (OM), management procedure (MP), and performance
  indicator (PI). Dimensions: c(`nOM`, `nMP`, and `nPI`). All PI values
  must range between 0 and 1 or 0 and 100. If all values are \<= 1, they
  will be multiplied by 100 in the plot. Dimensions: c(`nOM`, `nMP`, and
  `nPI`)

- `Preset`:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- `Misc`:

  A named list for additional miscellaneous information.

## See also

[Spider](https://slick.bluematterscience.com/reference/Spider-methods.md),
[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
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

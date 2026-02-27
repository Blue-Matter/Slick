# S4 class `Tradeoff`

Objects of class `Tradeoff` are used to store information for the
Tradeoff plot. Like all S4 objects in `Slick`, slots in this object can
be accessed and assigned using functions corresponding to slot name. See
[`Tradeoff()`](https://slick.bluematterscience.com/reference/Tradeoff-methods.md)
and the the `See Also` section below.

## Details

Objects of class `Tradeoff` are created with
[`Tradeoff()`](https://slick.bluematterscience.com/reference/Tradeoff-methods.md)

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

  A 3 dimensional numeric array with the stochastic performance
  indicator values for operating model (OM), management procedure (MP),
  and performance indicator (PI). Dimensions: c(`nOM`, `nMP`, and `nPI`)

- `Preset`:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- `Misc`:

  A named list for additional miscellaneous information.

## See also

[`Tradeoff()`](https://slick.bluematterscience.com/reference/Tradeoff-methods.md),
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

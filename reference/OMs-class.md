# `OMs` S4 class and functions

An object of class `OMs` contains information about the operating models
(MPs) in the
[`Slick()`](https://slick.bluematterscience.com/reference/Slick-class.md)
object. Like all S4 objects in `Slick`, slots in this object can be
accessed and assigned using functions corresponding to slot name. See
[`OMs()`](https://slick.bluematterscience.com/reference/OMs-methods.md)
and the the `See Also` section below.

## Details

### Multi-Language Support

Use a named list to use multi-languages in `Factors`

### Factors

`Factors` can be accessed and assigned using `Factors(myslick)` and
`Factors(myslick) <- data.frame()` respectively.

The `Factor` column should be character strings with the name of each
factor, while the `Level` column is a `numeric` or `character` value
with the level for the corresponding factor.

The `Description` column is a description for each row, i.e., a unique
factor and level. See `Examples`.

### Design

The `Design` matrix is `nOM` rows and `nFactor` columns. The values in
each column should either be `numeric` values indicating the levels for
the corresponding factor, or the actual level values (i.e.,
`Factors$Level`) that correspond to each OM. See `Examples`.

### Preset

For `OMs` objects, `Preset` should be a named list, where each list
element represents a different preset button to be shown in the app by
the name of the list element, and each named list element should be a
list of length `nFactors`, with the list elements for each Factor
containing numeric values indicating the levels to include for that
factor. See `Examples`

## Slots

- `Factors`:

  A `data.frame` with column headings `Factor`, `Level`, and
  `Description`. See `Details`

- `Design`:

  A `data.frame` with `nFactor` columns (i.e.,
  `length(unique(Factors$Factor))`), and `nOM` rows. See `Details`

- `Preset`:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).. See
  `Details` and `Examples`

## See also

[`OMs()`](https://slick.bluematterscience.com/reference/OMs-methods.md),
[`Factors()`](https://slick.bluematterscience.com/reference/Factors.md),
[`Design()`](https://slick.bluematterscience.com/reference/Design.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)

## Examples

``` r
# Create Object
oms <- OMs()

# Specify Factors
Factors(oms) <- data.frame(Factor='M',
                           Level=c('Base', 'Low M', 'High M'),
                           Description=c('Base Case',
                                         'Lower Natural Mortality',
                                         'Higher Natural Mortality')
)

Factors(oms)
#>   Factor  Level              Description
#> 1      M   Base                Base Case
#> 2      M  Low M  Lower Natural Mortality
#> 3      M High M Higher Natural Mortality

# OM Design

Design(oms) <- data.frame(M=c('Base', 'Low M', 'High M'))

# Add names for OMs
rownames(Design(oms)) <- c('Base Case', 'Less Productive', 'More Productive')

Design(oms)
#>                      M
#> Base Case         Base
#> Less Productive  Low M
#> More Productive High M

# Preset

Preset(oms) <- list('Base Case'=list(1),
                    'Low M' = list(2),
                    'High M' = list(3),
                    'All'= list(1:3)
)

# Create Slick Object
myslick <- Slick()

# Add OMs to Slick Object
OMs(myslick) <- oms
```

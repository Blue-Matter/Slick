# FilterSlick

Filter a Slick Object

## Usage

``` r
FilterSlick(slick = NULL, MPs = NULL, OMs = NULL, PIs = NULL, plot = NULL)
```

## Arguments

- slick:

  An object of class `Slick`

- MPs:

  Numeric values of the MPs to keep. Default NULL keeps all MPs.

- OMs:

  Numeric values of the OMs to keep (rows of `OM@Design`). Default NULL
  keeps all OMs.

- PIs:

  Numeric values of the PIs in `plot` to keep. Default NULL keeps all
  PIs.

- plot:

  The plot to filter the PIs. One of: `Timeseries`, `Boxplot`, `Kobe`,
  `Quilt`, `Spider`, or`Tradeoff`

## Value

A filtered Slick Object

## Details

Filter a Slick Object by management procedures (MPs), operating models
(OMs), and performance indicators (PIs) for a given plot

## Examples

``` r
if (FALSE) { # interactive()
slick <- Slick() # a completed slick object
boxplot_OM_1 <- FilterSlick(slick, OMs=1, plot='boxplot')


}
```

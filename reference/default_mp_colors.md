# Set Default Colors for the MPs

Requires the `colorspace` package

## Usage

``` r
default_mp_colors(nMP)
```

## Arguments

- nMP:

  The number of management procedures

## Value

A character vector of length `nMP` with color hex codes

## Examples

``` r
cols <- default_mp_colors(4)
cols
#> [1] "#C87A8A" "#909646" "#00A396" "#9189C7"
```

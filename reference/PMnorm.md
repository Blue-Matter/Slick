# Normalize performance metric values in a Slick object

A function that converts deterministic or stochastic performance metrics
to the range 0-100 and optionally inverts these

## Usage

``` r
PMnorm(obj, det = TRUE, inv = NULL)
```

## Arguments

- obj:

  An object of class 'slick'

- det:

  logical, should the normalization be applied to the deterministic
  performance metrics (or, if false the stochastic ones)

- inv:

  A logical vector nPM long. If true, the PM will be inverted
  (100-value).

## Value

An object of class
[Slick](https://slick.bluematterscience.com/reference/Slick-class.md)

## Author

T. Carruthers

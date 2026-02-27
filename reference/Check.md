# Check an object for errors or issues

Checks S4 objects to check for warnings and errors

## Usage

``` r
Check(object, ...)

# S4 method for class 'Boxplot'
Check(object, req_dims = c(NA, NA, NA, NA))

# S4 method for class 'Kobe'
Check(object)

# S4 method for class 'MPs'
Check(object)

# S4 method for class 'OMs'
Check(object)

# S4 method for class 'Quilt'
Check(object)

# S4 method for class 'Tradeoff'
Check(object)

# S4 method for class 'Timeseries'
Check(object)

# S4 method for class 'Spider'
Check(object)

# S4 method for class 'Slick'
Check(object)
```

## Arguments

- object:

  An object of class:
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md),
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md),
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)
  or the six chart types:
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md),
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md),
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md),
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md),
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md),
  and
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md).

- ...:

  Additional arguments

- req_dims:

  Required dimensions for `Value` slot. Used internally

## Value

Prints messages to the console

## Methods (by class)

- `Check(Boxplot)`: Check
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  objects for errors

- `Check(Kobe)`: Check
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  objects for errors

- `Check(MPs)`: Check
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  objects for errors

- `Check(OMs)`: Check
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)
  objects for errors

- `Check(Quilt)`: Check
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  objects for errors

- `Check(Tradeoff)`: Check
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  objects for errors

- `Check(Timeseries)`: Check
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  objects for errors

- `Check(Spider)`: Check
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  objects for errors

- `Check(Slick)`: Check
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  objects for errors

## Examples

``` r
slick <- Slick()
Check(slick)
#> 
#> ── Checking: "Slick" ──
#> 
#> ── Checking: "MPs" 
#> ℹ Object is empty
#> 
#> ── Checking: "OMs" 
#> ℹ Object is empty
#> 
#> ── Checking: "Boxplot" 
#> ℹ Object is empty
#> 
#> ── Checking: "Quilt" 
#> ℹ Object is empty
#> 
#> ── Checking: "Kobe" 
#> ℹ Object is empty
#> 
#> ── Checking: "Spider" 
#> ℹ Object is empty
#> 
#> ── Checking: "Timeseries" 
#> ℹ Object is empty
#> 
#> ── Checking: "Tradeoff" 
#> ℹ Object is empty
```

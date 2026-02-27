# Assign or access `RefPoints` for a valid object class

Assign or access `RefPoints` for a valid object class

## Usage

``` r
RefPoints(object)

RefPoints(object) <- value

# S4 method for class 'Timeseries'
RefPoints(object)

# S4 method for class 'Timeseries'
RefPoints(object) <- value
```

## Arguments

- object:

  An object of class
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)

- value:

  A `list`, formatted to match the class of `object`. See the
  documentation for corresponding `object` class for more details.

## Value

Returns a list object with the contents of the `RefPoints` slot of
[`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md)
objects

## Methods (by class)

- `RefPoints(Timeseries)`: Return `RefPoints` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `RefPoints(Timeseries) <- value`: Assign `RefPoints` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

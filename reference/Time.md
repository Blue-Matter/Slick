# Access or assign `Time` for object of class `Kobe` or `Timeseries`

Access or assign `Time` for object of class `Kobe` or `Timeseries`

## Usage

``` r
Time(object)

Time(object) <- value

# S4 method for class 'Kobe'
Time(object)

# S4 method for class 'Kobe'
Time(object) <- value

# S4 method for class 'Timeseries'
Time(object)

# S4 method for class 'Timeseries'
Time(object) <- value
```

## Arguments

- object:

  A
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  or
  [`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md)
  class object

- value:

  Value to assign to `Time`

## Value

Returns a numeric vector with values from the `Time` slot in
[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md)
and
[`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md)
objects

## Methods (by class)

- `Time(Kobe)`: Return `Time` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Time(Kobe) <- value`: Assign `Time` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Time(Timeseries)`: Return `Time` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Time(Timeseries) <- value`: Assign `Time` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

## Functions

- `Time(object) <- value`: Assign `value` to `object@Time`

# Assign or access `Value` for a valid object class

Assign or access `Value` for a valid object class

## Usage

``` r
Value(object)

Value(object) <- value

# S4 method for class 'Boxplot'
Value(object)

# S4 method for class 'Boxplot'
Value(object) <- value

# S4 method for class 'Kobe'
Value(object)

# S4 method for class 'Kobe'
Value(object) <- value

# S4 method for class 'Quilt'
Value(object)

# S4 method for class 'Quilt'
Value(object) <- value

# S4 method for class 'Tradeoff'
Value(object)

# S4 method for class 'Tradeoff'
Value(object) <- value

# S4 method for class 'Timeseries'
Value(object)

# S4 method for class 'Timeseries'
Value(object) <- value

# S4 method for class 'Spider'
Value(object)

# S4 method for class 'Spider'
Value(object) <- value
```

## Arguments

- object:

  An object of class
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md),
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md),
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md),
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md),
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md),
  or
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)

- value:

  An `array`, formatted to match the class of `object`. See the
  documentation for corresponding `object` class for more details.

## Value

Returns a numeric array with the contents of the `Value` slot of
`object`

## Methods (by class)

- `Value(Boxplot)`: Return `Value` from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Value(Boxplot) <- value`: Assign `Value` to a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Value(Kobe)`: Return `Value` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Value(Kobe) <- value`: Assign `Value` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Value(Quilt)`: Return `Value` from a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Value(Quilt) <- value`: Assign `Value` to a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Value(Tradeoff)`: Return `Value` from a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Value(Tradeoff) <- value`: Assign `Value` to a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Value(Timeseries)`: Return `Value` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Value(Timeseries) <- value`: Assign `Value` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Value(Spider)`: Return `Value` from a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Value(Spider) <- value`: Assign `Value` to a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

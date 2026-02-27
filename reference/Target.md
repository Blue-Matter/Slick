# Access or assign `Target` and `Limit` for object of class `Kobe` or `Timeseries`

Access or assign `Target` and `Limit` for object of class `Kobe` or
`Timeseries`

## Usage

``` r
Limit(object)

Limit(object) <- value

Target(object)

Target(object) <- value

# S4 method for class 'Kobe'
Limit(object)

# S4 method for class 'Kobe'
Limit(object) <- value

# S4 method for class 'Kobe'
Target(object)

# S4 method for class 'Kobe'
Target(object) <- value

# S4 method for class 'Timeseries'
Limit(object)

# S4 method for class 'Timeseries'
Limit(object) <- value

# S4 method for class 'Timeseries'
Target(object)

# S4 method for class 'Timeseries'
Target(object) <- value
```

## Arguments

- object:

  A
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  or
  [`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md)
  class object

- value:

  Value to assign to `Target`

## Value

Returns a numeric vector with the contents of the `Target` or `Limit`
slots of
[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md)
and
[`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md)
objects

## Methods (by class)

- `Limit(Kobe)`: Return `Limit` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Limit(Kobe) <- value`: Assign `Limit` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Target(Kobe)`: Return `Target` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Target(Kobe) <- value`: Assign `Target` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Limit(Timeseries)`: Return `Limit` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Limit(Timeseries) <- value`: Assign `Limit` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Target(Timeseries)`: Return `Target` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Target(Timeseries) <- value`: Assign `Target` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

## Functions

- `Target(object) <- value`: Assign `value` to `object@Target`

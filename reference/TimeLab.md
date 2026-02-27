# Access or assign `TimeLab` in a `Kobe` or `Timeseries` object

Access or assign `TimeLab` in a `Kobe` or `Timeseries` object

## Usage

``` r
TimeLab(object, lang = "en")

TimeLab(object) <- value

# S4 method for class 'Kobe'
TimeLab(object, lang = "en")

# S4 method for class 'Kobe'
TimeLab(object) <- value

# S4 method for class 'Timeseries'
TimeLab(object, lang = "en")

# S4 method for class 'Timeseries'
TimeLab(object) <- value
```

## Arguments

- object:

  A
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  or
  [Timeseries](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- lang:

  Optional text string specifying the language (if available). Either
  'en', 'es', 'fr', or 'pt' for English, Spanish, French, or Portuguese
  respectively

- value:

  A character string to assign to `TimeLab` in `object`.

## Value

Returns a character string from the `TimeLab` slot in
[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md)
and
[`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md)
objects

## Methods (by class)

- `TimeLab(Kobe)`: Return `TimeLab` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `TimeLab(Kobe) <- value`: Assign `TimeLab` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `TimeLab(Timeseries)`: Return `TimeLab` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `TimeLab(Timeseries) <- value`: Assign `TimeLab` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

# Access or assign the default selections for the plots in the `App()`

Access or assign the default selections for the plots in the
[`App()`](https://slick.bluematterscience.com/reference/App.md)

## Usage

``` r
Defaults(object)

Defaults(object) <- value

# S4 method for class 'Boxplot'
Defaults(object)

# S4 method for class 'Boxplot'
Defaults(object) <- value

# S4 method for class 'Kobe'
Defaults(object)

# S4 method for class 'Kobe'
Defaults(object) <- value
```

## Arguments

- object:

  A
  [`Boxplot()`](https://slick.bluematterscience.com/reference/Boxplot-methods.md)
  or
  [`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md)
  object

- value:

  A list of default selections for the
  [`App()`](https://slick.bluematterscience.com/reference/App.md)

## Value

Returns a list object with default selections from the `Defaults` slot
of
[`Boxplot()`](https://slick.bluematterscience.com/reference/Boxplot-methods.md)
and
[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md)
objects.

## Methods (by class)

- `Defaults(Boxplot)`: Defaults `Code` from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Defaults(Boxplot) <- value`: Assign `Defaults` to a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Defaults(Kobe)`: Defaults `Code` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Defaults(Kobe) <- value`: Assign `Defaults` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

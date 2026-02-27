# Return `Code`, `Label`, `Description` and other information from an object

Return `Code`, `Label`, `Description` and other information from an
object

## Usage

``` r
Metadata(object, lang = "en")

Metadata(object) <- value

# S4 method for class 'Boxplot'
Metadata(object, lang = "en")

# S4 method for class 'Boxplot'
Metadata(object) <- value

# S4 method for class 'Kobe'
Metadata(object, lang = "en")

# S4 method for class 'Kobe'
Metadata(object) <- value

# S4 method for class 'MPs'
Metadata(object, lang = "en")

# S4 method for class 'MPs'
Metadata(object) <- value

# S4 method for class 'Quilt'
Metadata(object, lang = "en")

# S4 method for class 'Quilt'
Metadata(object) <- value

# S4 method for class 'Tradeoff'
Metadata(object, lang = "en")

# S4 method for class 'Tradeoff'
Metadata(object) <- value

# S4 method for class 'Timeseries'
Metadata(object, lang = "en")

# S4 method for class 'Timeseries'
Metadata(object) <- value

# S4 method for class 'Spider'
Metadata(object, lang = "en")

# S4 method for class 'Spider'
Metadata(object) <- value

# S4 method for class 'Slick'
Metadata(object, lang = "en")
```

## Arguments

- object:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md),
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md),
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md),
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md),
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md),
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md),
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md),
  or
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- lang:

  Optional text string specifying the language (if available). Either
  'en', 'es', 'fr', or 'pt' for English, Spanish, French, or Portuguese
  respectively

- value:

  Replacement value for `Metadata()` in the corresponding `object`. See
  help documentation for the relevant object class for details.

## Value

`A data.frame`

A `data.frame`

## Methods (by class)

- `Metadata(Boxplot)`: Return Metadata for
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  objects

- `Metadata(Boxplot) <- value`: Assign Metadata for
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  objects

- `Metadata(Kobe)`: Return Metadata for
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  objects

- `Metadata(Kobe) <- value`: Assign Metadata for
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  objects

- `Metadata(MPs)`: Return Metadata for
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  objects

- `Metadata(MPs) <- value`: Assign Metadata for
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  objects

- `Metadata(Quilt)`: Return Metadata for
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  objects

- `Metadata(Quilt) <- value`: Assign Metadata for
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  objects

- `Metadata(Tradeoff)`: Return Metadata for
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  objects

- `Metadata(Tradeoff) <- value`: Assign Metadata for
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  objects

- `Metadata(Timeseries)`: Return Metadata for
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  objects

- `Metadata(Timeseries) <- value`: Assign Metadata for
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  objects

- `Metadata(Spider)`: Return Metadata for
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  objects

- `Metadata(Spider) <- value`: Assign Metadata for
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  objects

- `Metadata(Slick)`: Return `Author`, `Email`, and `Institution` from
  [`Slick()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  objects

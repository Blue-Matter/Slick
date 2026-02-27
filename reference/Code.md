# Access or assign `Code`, `Label`, and `Description` for a valid object class

Access or assign `Code`, `Label`, and `Description` for a valid object
class

## Usage

``` r
Code(object, lang = "en")

Code(object) <- value

Description(object, lang = "en")

Description(object) <- value

Label(object, lang = "en")

Label(object) <- value

# S4 method for class 'Boxplot'
Code(object, lang = "en")

# S4 method for class 'Boxplot'
Code(object) <- value

# S4 method for class 'Boxplot'
Description(object, lang = "en")

# S4 method for class 'Boxplot'
Label(object, lang = "en")

# S4 method for class 'Boxplot'
Label(object) <- value

# S4 method for class 'Boxplot'
Description(object) <- value

# S4 method for class 'Boxplot'
Misc(object)

# S4 method for class 'Boxplot'
Misc(object) <- value

# S4 method for class 'Kobe'
Code(object, lang = "en")

# S4 method for class 'Kobe'
Code(object) <- value

# S4 method for class 'Kobe'
Description(object, lang = "en")

# S4 method for class 'Kobe'
Description(object) <- value

# S4 method for class 'Kobe'
Label(object, lang = "en")

# S4 method for class 'Kobe'
Label(object) <- value

# S4 method for class 'Kobe'
Misc(object)

# S4 method for class 'Kobe'
Misc(object) <- value

# S4 method for class 'MPs'
Code(object, lang = "en")

# S4 method for class 'MPs'
Code(object) <- value

# S4 method for class 'MPs'
Label(object, lang = "en")

# S4 method for class 'MPs'
Label(object) <- value

# S4 method for class 'MPs'
Description(object, lang = "en")

# S4 method for class 'MPs'
Description(object) <- value

# S4 method for class 'Quilt'
Code(object, lang = "en")

# S4 method for class 'Quilt'
Code(object) <- value

# S4 method for class 'Quilt'
Description(object, lang = "en")

# S4 method for class 'Quilt'
Description(object) <- value

# S4 method for class 'Quilt'
Label(object, lang = "en")

# S4 method for class 'Quilt'
Label(object) <- value

# S4 method for class 'Quilt'
Misc(object)

# S4 method for class 'Quilt'
Misc(object) <- value

# S4 method for class 'Tradeoff'
Code(object, lang = "en")

# S4 method for class 'Tradeoff'
Code(object) <- value

# S4 method for class 'Tradeoff'
Description(object, lang = "en")

# S4 method for class 'Tradeoff'
Description(object) <- value

# S4 method for class 'Tradeoff'
Misc(object)

# S4 method for class 'Tradeoff'
Misc(object) <- value

# S4 method for class 'Tradeoff'
Label(object, lang = "en")

# S4 method for class 'Tradeoff'
Label(object) <- value

# S4 method for class 'Timeseries'
Code(object, lang = "en")

# S4 method for class 'Timeseries'
Code(object) <- value

# S4 method for class 'Timeseries'
Description(object, lang = "en")

# S4 method for class 'Timeseries'
Description(object) <- value

# S4 method for class 'Timeseries'
Label(object, lang = "en")

# S4 method for class 'Timeseries'
Label(object) <- value

# S4 method for class 'Timeseries'
Misc(object)

# S4 method for class 'Timeseries'
Misc(object) <- value

# S4 method for class 'Spider'
Code(object, lang = "en")

# S4 method for class 'Spider'
Code(object) <- value

# S4 method for class 'Spider'
Description(object, lang = "en")

# S4 method for class 'Spider'
Description(object) <- value

# S4 method for class 'Spider'
Label(object, lang = "en")

# S4 method for class 'Spider'
Label(object) <- value

# S4 method for class 'Spider'
Misc(object)

# S4 method for class 'Spider'
Misc(object) <- value
```

## Arguments

- object:

  An object of class
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md),
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md),
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md),
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md),
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md),
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md),
  or
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)

- lang:

  Optional text string specifying the language (if available). Either
  'en', 'es', 'fr', or 'pt' for English, Spanish, French, or Portuguese
  respectively

- value:

  A character vector or a named list for multi-language support

## Value

Returns character string or named list with the contents of the `Code`,
`Label`, or `Description` slot of `object`

## Details

`Code`, `Label`, and `Description` must all be equal length.

### Multi-Language Support

Text with multi-language supported can be provided as a named list.
Available languages:

- `en`: English (default)

- `es`: Spanish

- `fr`: French

- `pt`: Portuguese

## Methods (by class)

- `Code(Boxplot)`: Return `Code` from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Code(Boxplot) <- value`: Assign `Code` to a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Description(Boxplot)`: Return `Description` from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Label(Boxplot)`: Return `Label` from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Label(Boxplot) <- value`: Assign `Label` to a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Description(Boxplot) <- value`: Assign `Description` to a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Misc(Boxplot)`: Return `Misc` from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Misc(Boxplot) <- value`: Assign `Misc` to a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Code(Kobe)`: Return `Code` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Code(Kobe) <- value`: Assign `Code` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Description(Kobe)`: Return `Description` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Description(Kobe) <- value`: Assign `Description` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Label(Kobe)`: Return `Label` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Label(Kobe) <- value`: Assign `Label` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Misc(Kobe)`: Return `Misc` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Misc(Kobe) <- value`: Assign `Misc` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Code(MPs)`: Return `Code` from a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Code(MPs) <- value`: Assign `Code` to a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Label(MPs)`: Return `Label` from a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Label(MPs) <- value`: Assign `Label` to a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Description(MPs)`: Return `Description` from a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Description(MPs) <- value`: Assign `Description` to a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Code(Quilt)`: Return `Code` from a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Code(Quilt) <- value`: Assign `Code` to a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Description(Quilt)`: Return `Description` from a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Description(Quilt) <- value`: Assign `Description` to a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Label(Quilt)`: Return `Label` from a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Label(Quilt) <- value`: Assign `Label` to a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Misc(Quilt)`: Return `Misc` from a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Misc(Quilt) <- value`: Assign `Misc` to a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Code(Tradeoff)`: Return `Code` from a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Code(Tradeoff) <- value`: Assign `Code` to a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Description(Tradeoff)`: Return `Description` from a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Description(Tradeoff) <- value`: Assign `Description` to a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Misc(Tradeoff)`: Return `Misc` from a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Misc(Tradeoff) <- value`: Assign `Misc` to a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Label(Tradeoff)`: Return `Label` from a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Label(Tradeoff) <- value`: Assign `Label` to a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Code(Timeseries)`: Return `Code` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Code(Timeseries) <- value`: Assign `Code` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Description(Timeseries)`: Return `Description` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Description(Timeseries) <- value`: Assign `Description` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Label(Timeseries)`: Return `Label` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Label(Timeseries) <- value`: Assign `Label` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Misc(Timeseries)`: Return `Misc` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Misc(Timeseries) <- value`: Assign `Misc` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Code(Spider)`: Return `Code` from a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Code(Spider) <- value`: Assign `Code` to a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Description(Spider)`: Return `Description` from a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Description(Spider) <- value`: Assign `Description` to a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Label(Spider)`: Return `Label` from a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Label(Spider) <- value`: Assign `Label` to a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Misc(Spider)`: Return `Misc` from a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Misc(Spider) <- value`: Assign `Misc` to a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

## See also

`Label()`, `Description()`,
[`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md),
[`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md),
[`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md),
[`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md),
[`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md),
[`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md),
[`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)

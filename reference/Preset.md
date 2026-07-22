# Assign or access `Preset` for a valid object class

Assign or access `Preset` for a valid object class

## Usage

``` r
Preset(object)

Preset(object) <- value

# S4 method for class 'Boxplot'
Preset(object)

# S4 method for class 'Boxplot'
Preset(object) <- value

# S4 method for class 'Kobe'
Preset(object)

# S4 method for class 'Kobe'
Preset(object) <- value

# S4 method for class 'MPs'
Preset(object)

# S4 method for class 'MPs'
Preset(object) <- value

# S4 method for class 'OMs'
Preset(object)

# S4 method for class 'OMs'
Preset(object) <- value

# S4 method for class 'Quilt'
Preset(object)

# S4 method for class 'Quilt'
Preset(object) <- value

# S4 method for class 'Tradeoff'
Preset(object)

# S4 method for class 'Tradeoff'
Preset(object) <- value

# S4 method for class 'Timeseries'
Preset(object)

# S4 method for class 'Timeseries'
Preset(object) <- value

# S4 method for class 'Spider'
Preset(object)

# S4 method for class 'Spider'
Preset(object) <- value
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

  A `list`, formatted to match the class of `object`. See the
  documentation for corresponding `object` class for more details.

## Value

Returns a list object from the `Preset` slot in `object`

## Methods (by class)

- `Preset(Boxplot)`: Return `Preset` from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Preset(Boxplot) <- value`: Assign `Preset` slot from a
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)
  object

- `Preset(Kobe)`: Return `Preset` from a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Preset(Kobe) <- value`: Assign `Preset` to a
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)
  object

- `Preset(MPs)`: Return `Preset` from a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Preset(MPs) <- value`: Assign `Preset` to a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Preset(OMs)`: Return `Preset` from a
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)
  object

  For backward compatibility, a legacy `Preset` (nested
  one-list-element-per- `Factor` format) is auto-converted to the
  current flat `Design`-row-index format on read. This isn't persisted
  back into `object` here;
  [`Update()`](https://slick.bluematterscience.com/reference/Update.md)
  does that permanently for a `Slick` object loaded via the app.

- `Preset(OMs) <- value`: Assign `Preset` to an `OMs-class()` object

- `Preset(Quilt)`: Return `Preset` from a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Preset(Quilt) <- value`: Assign `Preset` to a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Preset(Tradeoff)`: Return `Preset` from a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Preset(Tradeoff) <- value`: Assign `Preset` to a
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)
  object

- `Preset(Timeseries)`: Return `Preset` from a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Preset(Timeseries) <- value`: Assign `Preset` to a
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)
  object

- `Preset(Spider)`: Return `Preset` from a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

- `Preset(Spider) <- value`: Assign `Preset` slot from a
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)
  object

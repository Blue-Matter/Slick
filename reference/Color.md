# Access or assign `Color` for `MPs` and `Quilt` objects

Access or assign `Color` for `MPs` and `Quilt` objects

## Usage

``` r
Color(object)

Color(object) <- value

# S4 method for class 'MPs'
Color(object)

# S4 method for class 'MPs'
Color(object) <- value

# S4 method for class 'Quilt'
Color(object)

# S4 method for class 'Quilt'
Color(object) <- value
```

## Arguments

- object:

  An
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  or
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- value:

  A character vector formatted to match the class of `object`. See the
  documentation for corresponding `object` class for more details.

## Value

Returns a character string with the contents of the `Color` slot of
`object`

## Methods (by class)

- `Color(MPs)`: Return `Color` from a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Color(MPs) <- value`: Preset Assign `Color` to a
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

- `Color(Quilt)`: Return `Color` from a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

- `Color(Quilt) <- value`: Preset Assign `Color` to a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

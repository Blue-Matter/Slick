# Return the Factors matrix from an `OMs` object

Return the Factors matrix from an `OMs` object

## Usage

``` r
Factors(object, lang = "en")

Factors(object) <- value

# S4 method for class 'OMs'
Factors(object, lang = "en")

# S4 method for class 'OMs'
Factors(object) <- value

# S4 method for class 'Slick'
Factors(object, lang = "en")

# S4 method for class 'Slick'
Factors(object) <- value
```

## Arguments

- object:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- lang:

  Optional text string specifying the language (if available). Either
  'en', 'es', 'fr', or 'pt' for English, Spanish, French, or Portuguese
  respectively

- value:

  A `data.frame` with the Factors

## Value

The Design matrix from an `OMs` object in a `Slick` object

## Methods (by class)

- `Factors(OMs)`: Return the operating model `Factors` table from a
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)
  object

- `Factors(OMs) <- value`: Assign the operating model `Factors` table to
  a
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)
  object

- `Factors(Slick)`: Access the operating model `Factors` table from a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `Factors(Slick) <- value`: Assign the operating model `Factors` table
  to a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

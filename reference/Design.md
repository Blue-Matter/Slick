# Return the Design matrix from an `OMs` object

Return the Design matrix from an `OMs` object

## Usage

``` r
Design(object)

Design(object) <- value

# S4 method for class 'OMs'
Design(object)

# S4 method for class 'OMs'
Design(object) <- value

# S4 method for class 'Slick'
Design(object)

# S4 method for class 'Slick'
Design(object) <- value
```

## Arguments

- object:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- value:

  A `data.frame` with the Design matrix

## Value

The Design matrix from an `OMs` object in a `Slick` object

## Methods (by class)

- `Design(OMs)`: Return the operating model `Design` matrix from a
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)
  object

- `Design(OMs) <- value`: Assign the operating model `Design` matrix to
  a
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)
  object

- `Design(Slick)`: Access the operating model `Design` matrix from a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `Design(Slick) <- value`: Assign the operating model `Design` matrix
  to a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

# Create a `Slick` class object

The `Slick` class is the main object class used in the `Slick` package.
It contains sub-objects for the management procedures
[`MPs()`](https://slick.bluematterscience.com/reference/MPs-methods.md),
operating models
[`OMs()`](https://slick.bluematterscience.com/reference/OMs-methods.md),
and the six chart types:
[`Boxplot()`](https://slick.bluematterscience.com/reference/Boxplot-methods.md),
[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md),
[`Quilt()`](https://slick.bluematterscience.com/reference/Quilt-methods.md),
[`Spider()`](https://slick.bluematterscience.com/reference/Spider-methods.md),
[`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md),
and
[`Tradeoff()`](https://slick.bluematterscience.com/reference/Tradeoff-methods.md),
as well as metadata information for the `Slick` object such as `Title`,
`Author`, and `Introduction`.

## Usage

``` r
Slick(
  Title = "",
  Subtitle = "",
  Date = Sys.Date(),
  Author = "",
  Email = "",
  Institution = "",
  Introduction = "",
  MPs = NULL,
  OMs = NULL,
  Boxplot = NULL,
  Kobe = NULL,
  Quilt = NULL,
  Spider = NULL,
  Timeseries = NULL,
  Tradeoff = NULL
)

Title(object, lang = "en", markdown = FALSE)

Title(object) <- value

Subtitle(object, lang = "en", markdown = FALSE)

Subtitle(object) <- value

Date(object)

Date(object) <- value

Author(object, markdown = FALSE)

Author(object) <- value

Email(object, markdown = FALSE)

Email(object) <- value

Institution(object, lang = "en", markdown = FALSE)

Institution(object) <- value

Introduction(object, lang = "en", markdown = FALSE)

Introduction(object) <- value
```

## Arguments

- Title:

  Title for the `Slick` object. A character string. For multiple
  languages, use a named list with names: `en`, `es`, `fr`, or `pt` for
  the supported languages.

- Subtitle:

  Subtitle for the `Slick` object. A character string or a named list
  with languages: `en`, `es`, `fr`, `pt`

- Date:

  Date the Slick object was created. Text in format 'YYYY-MM-DD' or
  class `Date` e.g.,
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

- Author:

  A character vector with Author(s) names. The length of the vector
  should equal the number of authors.

- Email:

  A character vector with email addresses for the author(s). Must be
  same length as `Author`. Can include Markdown.

- Institution:

  A character vector with institution details for the author(s). Must be
  same length as `Author`. Can include Markdown.

- Introduction:

  Introduction text for the `Slick` object. Supports all markdown
  formatting. Character string, must be length 1. For multiple
  languages, use a named list with names: `en`, `es`, `fr`, `pt` for the
  supported languages.

- MPs:

  An object of class
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)

- OMs:

  An object of class
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)

- Boxplot:

  An object of class
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)

- Kobe:

  An object of class
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)

- Quilt:

  An object of class
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)

- Spider:

  An object of class
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)

- Timeseries:

  An object of class
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)

- Tradeoff:

  An object of class
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)

- object:

  A `Slick-class()` object

- lang:

  Optional text string specifying the language (if available). Either
  'en', 'es', 'fr', or 'pt' for English, Spanish, French, or Portuguese
  respectively

- markdown:

  Logical. Process markdown?

- value:

  The value to assign to the object. See `Slots` for format of the
  relevant object class

## Value

A `Slick` object

## Details

Objects of class `Slick` are created with `Slick()`.

Like all S4 objects in `Slick`, slots in this object can be accessed and
assigned using functions corresponding to slot name. See `Usage` and
`Functions` section.

### Multi-Language Support

Text with multi-language supported can be provided as a named list.
Available languages:

- `en`: English (default)

- `es`: Spanish

- `fr`: French

- `pt`: Portuguese

All functions with the exception of `Date` support Markdown.

## Functions

- `Slick()`: Create a `Slick-class()` object

- `Title()`: Access `Title`, Multi-language support

- `Title(object) <- value`: Assign `Title`, Multi-language support

- `Subtitle()`: Access `Subtitle`, Multi-language support

- `Subtitle(object) <- value`: Assign `Subtitle`, Multi-language support

- `Date()`: Access `Date`

- `Date(object) <- value`: Assign `Date`

- `Author()`: Access `Author`

- `Author(object) <- value`: Assign `Author`

- `Email()`: Access `Email`

- `Email(object) <- value`: Assign `Email`

- `Institution()`: Access `Institution`

- `Institution(object) <- value`: Assign `Institution`

- `Introduction()`: Access `Introduction`

- `Introduction(object) <- value`: Assign `Introduction`, can include
  Markdown. See `Examples`

## Slots

- `Title`:

  Title for the `Slick` object. A character string. For multiple
  languages, use a named list with names: `en`, `es`, `fr`, or `pt` for
  the supported languages.

- `Subtitle`:

  Subtitle for the `Slick` object. A character string or a named list
  with languages: `en`, `es`, `fr`, `pt`

- `Date`:

  Date the Slick object was created. Text in format 'YYYY-MM-DD' or
  class `Date` e.g.,
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)

- `Author`:

  A character vector with Author(s) names. The length of the vector
  should equal the number of authors.

- `Email`:

  A character vector with email addresses for the author(s). Must be
  same length as `Author`. Can include Markdown.

- `Institution`:

  A character vector with institution details for the author(s). Must be
  same length as `Author`. Can include Markdown.

- `Introduction`:

  Introduction text for the `Slick` object. Supports all markdown
  formatting. Character string, must be length 1. For multiple
  languages, use a named list with names: `en`, `es`, `fr`, `pt` for the
  supported languages.

- `MPs`:

  An object of class
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)

- `OMs`:

  An object of class
  [`OMs-class()`](https://slick.bluematterscience.com/reference/OMs-class.md)

- `Boxplot`:

  An object of class
  [`Boxplot-class()`](https://slick.bluematterscience.com/reference/Boxplot-class.md)

- `Kobe`:

  An object of class
  [`Kobe-class()`](https://slick.bluematterscience.com/reference/Kobe-class.md)

- `Quilt`:

  An object of class
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)

- `Spider`:

  An object of class
  [`Spider-class()`](https://slick.bluematterscience.com/reference/Spider-class.md)

- `Timeseries`:

  An object of class
  [`Timeseries-class()`](https://slick.bluematterscience.com/reference/Timeseries-class.md)

- `Tradeoff`:

  An object of class
  [`Tradeoff-class()`](https://slick.bluematterscience.com/reference/Tradeoff-class.md)

## See also

[`MPs()`](https://slick.bluematterscience.com/reference/MPs-methods.md),
[`OMs()`](https://slick.bluematterscience.com/reference/OMs-methods.md),
[`Boxplot()`](https://slick.bluematterscience.com/reference/Boxplot-methods.md),
[`Kobe()`](https://slick.bluematterscience.com/reference/Kobe-methods.md),
[`Quilt()`](https://slick.bluematterscience.com/reference/Quilt-methods.md),
[`Spider()`](https://slick.bluematterscience.com/reference/Spider-methods.md),
[`Timeseries()`](https://slick.bluematterscience.com/reference/Timeseries-methods.md),
[`Tradeoff()`](https://slick.bluematterscience.com/reference/Tradeoff-methods.md),
[`Check()`](https://slick.bluematterscience.com/reference/Check.md),
`Title()`, `Subtitle()`, `Date()`, `Author()`, `Email()`,
`Institution()`, `Introduction()`

## Examples

``` r

# Assign values to a new `Slick` object
slick <- Slick()

Title(slick) <- 'An Example Slick Object'
Subtitle(slick) <- ""
Date(slick) <- Sys.Date()
Author(slick) <- 'Adrian Hordyk'
Email(slick) <-  "[mailto:adrian@bluematterscience.com](mailto:adrian@bluematterscience.com)"
Institution(slick) <- "[Blue Matter Science](bluematterscience.com)"

Introduction(slick) <- "This is the Introduction text"

# Access values from `Slick` object
Title(slick)
#> [1] "An Example Slick Object"
Subtitle(slick)
#> [1] ""
Date(slick)
#> [1] "2026-05-19"
Author(slick)
#> [1] "Adrian Hordyk"
Email(slick)
#> [1] "[mailto:adrian@bluematterscience.com](mailto:adrian@bluematterscience.com)"
Institution(slick)
#> [1] "[Blue Matter Science](bluematterscience.com)"
Introduction(slick)
#> [1] "This is the Introduction text"
```

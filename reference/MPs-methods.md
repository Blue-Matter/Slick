# Methods for Creating, Accessing and Assigning `MPs` objects

The `MPs` function is used both to create and modify an
[`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
object. and to access and assign `MPs` for an object of class
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md).
See `Details`.

## Usage

``` r
MPs(Code = "", Label = "", Description = "", Color = "", Preset = list())

MPs(object) <- value

# S4 method for class 'missing'
MPs()

# S4 method for class 'character_list'
MPs(Code = "", Label = "", Description = "", Color = "", Preset = list())

# S4 method for class 'Slick'
MPs(Code)

# S4 method for class 'Slick'
MPs(object) <- value
```

## Arguments

- Code:

  A *short* code for the Management Procedures in this `Slick` object. A
  character string length `nMP` or a named list for multi-language
  support. See `Details`

- Label:

  A short label for the Management Procedures in this `Slick` object.
  Can be longer than `Code` but recommended to keep short as possible so
  it shows clearly in plots and tables. A character string length `nMP`
  or a named list for multi-language support. See `Details`

- Description:

  A description for the Management Procedures in this `Slick` object.
  Can include Markdown, see `Examples`. A character string length `nMP`
  or a named list for multi-language support. See `Details`

- Color:

  A character vector of colors for the MPs.

- Preset:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

  Use [`Code()`](https://slick.bluematterscience.com/reference/Code.md),
  [`Label()`](https://slick.bluematterscience.com/reference/Code.md),
  [`Description()`](https://slick.bluematterscience.com/reference/Code.md),
  and
  [`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)
  to access and assign the values for an existing `MPs` object, see
  `Examples`.

- object:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- value:

  A
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object

## Details

Objects of class `MPs` are created with `MPs()`

## Functions

- `MPs(missing)`: Create an empty `MPs` object

- `MPs(character_list)`: Create a populated `MPs` object

- `MPs(Slick)`: Return an
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object from a
  [`Slick()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `MPs(Slick) <- value`: Assign an
  [`MPs-class()`](https://slick.bluematterscience.com/reference/MPs-class.md)
  object to a
  [`Slick()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

## See also

[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Color()`](https://slick.bluematterscience.com/reference/Color.md),
[`Metadata()`](https://slick.bluematterscience.com/reference/Metadata.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)

## Examples

``` r
myMPs <- MPs()
Code(myMPs) <- c('MP1', 'MP2', 'MP3')
Label(myMPs) <- c('Management Procedure 1',
                  'Management Procedure 2',
                  'Management Procedure 3')
Description(myMPs) <- c('This is the description for Management Procedure 1',
                        'This is the description for Management Procedure 2',
                        'This is the description for Management Procedure 3')

Preset(myMPs) <- list(All=1:3, FirstTwo=1:2)


myMPs
#> 
#> ── An object of class `MPs` ────────────────────────────────────────────────────
#> 
#> ── `Code` ──
#> 
#> 1 MP1
#> 2 MP2
#> 3 MP3
#> 
#> ── `Label` ──
#> 
#> 1 Management Procedure 1
#> 2 Management Procedure 2
#> 3 Management Procedure 3
#> 
#> ── `Description` ──
#> 
#> 1 This is the description for Management P ...
#> 2 This is the description for Management P ...
#> 3 This is the description for Management P ...
#> 
#> ── `Color` ──
#> 
#> 1 #C87A8A
#> 2 #6B9D59
#> 3 #5F96C2
#> 
#> ── `Preset` ──
#> 
#> ── All 
#> 1
#> 2
#> 3
#> 
#> ── FirstTwo 
#> 1
#> 2

# Multi-language
Description(myMPs) <- list(en=c('This is the English description for Management Procedure 1',
                             'This is the English description for Management Procedure 2',
                             'This is the English description for Management Procedure 3'),
                           es=c("This is the Spanish description for Management Procedure 1",
                             "This is the Spanish description for Management Procedure 2",
                             "This is the Spanish description for Management Procedure 3"),
                           fr=c("This is the French description for Management Procedure 1",
                             "This is the French description for Management Procedure 2",
                             "This is the French description for Management Procedure 3"),
                           pt=c("This is the Portuguese description for Management Procedure 1",
                                "This is the Portuguese description for Management Procedure 2",
                                "This is the Portuguese description for Management Procedure 3")
                           )

Metadata(myMPs)
#>   Code                  Label
#> 1  MP1 Management Procedure 1
#> 2  MP2 Management Procedure 2
#> 3  MP3 Management Procedure 3
#>                                                  Description   Color
#> 1 This is the English description for Management Procedure 1 #C87A8A
#> 2 This is the English description for Management Procedure 2 #6B9D59
#> 3 This is the English description for Management Procedure 3 #5F96C2
Metadata(myMPs, 'es')
#>   Code                  Label
#> 1  MP1 Management Procedure 1
#> 2  MP2 Management Procedure 2
#> 3  MP3 Management Procedure 3
#>                                                  Description   Color
#> 1 This is the Spanish description for Management Procedure 1 #C87A8A
#> 2 This is the Spanish description for Management Procedure 2 #6B9D59
#> 3 This is the Spanish description for Management Procedure 3 #5F96C2
Metadata(myMPs, 'fr')
#>   Code                  Label
#> 1  MP1 Management Procedure 1
#> 2  MP2 Management Procedure 2
#> 3  MP3 Management Procedure 3
#>                                                 Description   Color
#> 1 This is the French description for Management Procedure 1 #C87A8A
#> 2 This is the French description for Management Procedure 2 #6B9D59
#> 3 This is the French description for Management Procedure 3 #5F96C2
Metadata(myMPs, 'pt')
#>   Code                  Label
#> 1  MP1 Management Procedure 1
#> 2  MP2 Management Procedure 2
#> 3  MP3 Management Procedure 3
#>                                                     Description   Color
#> 1 This is the Portuguese description for Management Procedure 1 #C87A8A
#> 2 This is the Portuguese description for Management Procedure 2 #6B9D59
#> 3 This is the Portuguese description for Management Procedure 3 #5F96C2
```

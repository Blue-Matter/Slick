# `MPs` S4 class and functions

An object of class `MPs` contains information about the management
procedures (MPs) in a
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
object. Like all S4 objects in `Slick`, slots in this object can be
accessed and assigned using functions corresponding to slot name. See
[`MPs()`](https://slick.bluematterscience.com/reference/MPs-methods.md)
and the the `See Also` section below.

## Details

Objects of class `MPs` are created with
[`MPs()`](https://slick.bluematterscience.com/reference/MPs-methods.md)

### Multi-Language Support

Text with multi-language supported can be provided as a named list.
Available languages:

- `en`: English (default)

- `es`: Spanish

- `fr`: French

- `pt`: Portuguese

### Note

Character strings in `Code`, `Label`, and `Description` must all be same
length as the number of management procedures (`nMPs`) in the plot
objects `Boxplot`, `Kobe`, `Quilt`, `Spider`, `Timeseries`, and
`Tradeoff`.

## Slots

- `Code`:

  A *short* code for the Management Procedures in this `Slick` object. A
  character string length `nMP` or a named list for multi-language
  support. See `Details` *Required*

- `Label`:

  A short label for the Management Procedures in this `Slick` object.
  Can be longer than `Code` but recommended to keep short as possible so
  it shows clearly in plots and tables. A character string length `nMP`
  or a named list for multi-language support. See `Details` *Required*

- `Description`:

  A description for the Management Procedures in this `Slick` object.
  Can include Markdown, see `Examples`. A character string length `nMP`
  or a named list for multi-language support. See `Details`

- `Color`:

  A character vector of colors for the MPs. Defaults will be used if not
  populated

- `Preset`:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

## See also

[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
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

# Methods for Creating, Accessing and Assigning `Quilt` objects

The `Quilt` function is used both to create and modify an
[`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
object. and to access and assign `Quilt` for an object of class
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md).
See `Details`.

## Usage

``` r
Quilt(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Color = c("darkblue", "lightblue"),
  MinValue = as.numeric(NA),
  MaxValue = as.numeric(NA),
  Misc = list()
)

Quilt(Slick) <- value

# S4 method for class 'missing'
Quilt()

# S4 method for class 'character_list'
Quilt(
  Code = "",
  Label = "",
  Description = "",
  Value = array(),
  Preset = list(),
  Color = c("darkblue", "lightblue"),
  MinValue = as.numeric(NA),
  MaxValue = as.numeric(NA),
  Misc = list()
)

# S4 method for class 'Slick'
Quilt(Code)

# S4 method for class 'Slick'
Quilt(Slick) <- value
```

## Arguments

- Code:

  A *short* code for the Performance Indicators for this object. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- Label:

  A short label for the Performance Indicators for this object. Used to
  label axes on charts. Can be longer than `Code` but recommended to
  keep short as possible so it shows clearly in plots and tables. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- Description:

  A description for the Performance Indicators for this object. Can
  include Markdown, see `Examples`. A character string length `nPI` or a
  named list for multi-language support. See `Details`

- Value:

  A numeric array with the stochastic performance indicator values for
  each operating model (OM), management procedure (MP), and performance
  indicator (PI). Dimensions: c(`nOM`, `nMP`, and `nPI`). Alternatively,
  to calculate average over both simulations and Operating Models,
  `Value` can be a 4-dimensional array with dimensions: c(`nSim`, `nOM`,
  `nMP`, and `nPI`).

- Preset:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- Color:

  A character vector length 2 of colors for the maximum and minimum
  values in the chart.

- MinValue:

  Numeric vector length `nPI` with the minimum possible value for the
  respective PIs. Defaults to minimum PI value in `Value` (averaged
  across OMs in some cases)

- MaxValue:

  Numeric vector length `nPI` with the maximum possible value (i.e.,
  best performance) for the respective PIs. Defaults to maximum PI value
  in `Value` (averaged across OMs in some cases).

- Misc:

  A named list for additional miscellaneous information.

- Slick:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- value:

  A
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object

## Details

Objects of class `Quilt` are created with `Quilt()`

Use the
[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md),
[`Color()`](https://slick.bluematterscience.com/reference/Color.md),
[`MinValue()`](https://slick.bluematterscience.com/reference/MinValue.md),
and
[`MaxValue()`](https://slick.bluematterscience.com/reference/MinValue.md)
functions to access and assign the values for an existing `Quilt`
object, see `Examples`

### Multi-Language Support

Text with multi-language supported can be provided as a named list.
Available languages:

- `en`: English (default)

- `es`: Spanish

- `fr`: French

- `pt`: Portuguese

### Note

Character strings in `Code`, `Label`, and `Description` must all be same
length as the number of performance indicators (`nPIs`) in `Value`

## Functions

- `Quilt(missing)`: Create an empty `Quilt` object

- `Quilt(character_list)`: Create a populated `Quilt` object

- `Quilt(Slick)`: Return `Quilt` from a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- `Quilt(Slick) <- value`: Assign a
  [`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)
  object to a
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

## See also

[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Color()`](https://slick.bluematterscience.com/reference/Color.md),
[`Metadata()`](https://slick.bluematterscience.com/reference/Metadata.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md),
[`Color()`](https://slick.bluematterscience.com/reference/Color.md),
[`MinValue()`](https://slick.bluematterscience.com/reference/MinValue.md),
[`MaxValue()`](https://slick.bluematterscience.com/reference/MinValue.md)

## Examples

``` r
# Generate dummy values
nOM <- 2
nMP <- 4
nPI <- 4

values <- array(NA, dim=c(nOM, nMP, nPI))

pi_means <- runif(nPI, 5, 50)
for (om in 1:nOM) {
  for (mp in 1:nMP) {
    for (pi in 1:nPI) {
      values[om, mp, pi] <- rlnorm(1,log(pi_means[pi]), 0.4)
    }
  }
}

# Create and populate Object
quilt <- Quilt(Code=c('PI1', 'PI2', 'PI3', 'PI4'),
               Label=c('Performance Indicator 1',
                       'Performance Indicator 2',
                       'Performance Indicator 3',
                       'Performance Indicator 4'),
               Description = c('This is the description for PI 1',
                               'This is the description for PI 2',
                               'This is the description for PI 3',
                               'This is the description for PI 4'),
               Value=values)

# Check
Check(quilt)
#> 
#> ── Checking: "Quilt" ──
#> 
#> ✔ Complete

# Add to `Slick` object
slick <- Slick()
Quilt(slick) <- quilt

# Plots
plotQuilt(slick)
#> ℹ Note: `MPs` is empty. Using default MP names and colors

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"29.3\" data-max=\"59.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"34.4\" data-max=\"42.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"10.3\" data-max=\"33.2\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"8.4\" data-max=\"15\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[29.3,39.4,36.1,59.4],[34.4,38.4,35.2,42.8],[14.4,33.2,10.3,20.5],[9.83,8.4,10.2,15]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 29.3 ? \"#ADD8E680\" : value <= 32.31 ? \"#9DC4DD80\" : value <= 35.32 ? \"#8DB0D580\" : value <= 38.33 ? \"#7D9DCD80\" : value <= 41.34 ? \"#6E89C480\" : value <= 44.35 ? \"#5E75BC80\" : value <= 47.36 ? \"#4E62B480\" : value <= 50.37 ? \"#3E4EAC80\" : value <= 53.38 ? \"#2F3AA380\" : value <= 56.39 ? \"#1F279B80\" : value <= 59.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 34.4 ? \"#ADD8E680\" : value <= 35.24 ? \"#9DC4DD80\" : value <= 36.08 ? \"#8DB0D580\" : value <= 36.92 ? \"#7D9DCD80\" : value <= 37.76 ? \"#6E89C480\" : value <= 38.6 ? \"#5E75BC80\" : value <= 39.44 ? \"#4E62B480\" : value <= 40.28 ? \"#3E4EAC80\" : value <= 41.12 ? \"#2F3AA380\" : value <= 41.96 ? \"#1F279B80\" : value <= 42.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 10.3 ? \"#ADD8E680\" : value <= 12.59 ? \"#9DC4DD80\" : value <= 14.88 ? \"#8DB0D580\" : value <= 17.17 ? \"#7D9DCD80\" : value <= 19.46 ? \"#6E89C480\" : value <= 21.75 ? \"#5E75BC80\" : value <= 24.04 ? \"#4E62B480\" : value <= 26.33 ? \"#3E4EAC80\" : value <= 28.62 ? \"#2F3AA380\" : value <= 30.91 ? \"#1F279B80\" : value <= 33.2 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 8.4 ? \"#ADD8E680\" : value <= 9.06 ? \"#9DC4DD80\" : value <= 9.72 ? \"#8DB0D580\" : value <= 10.38 ? \"#7D9DCD80\" : value <= 11.04 ? \"#6E89C480\" : value <= 11.7 ? \"#5E75BC80\" : value <= 12.36 ? \"#4E62B480\" : value <= 13.02 ? \"#3E4EAC80\" : value <= 13.68 ? \"#2F3AA380\" : value <= 14.34 ? \"#1F279B80\" : value <= 15 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

# Alternative - include Simulation dimension

# Generate dummy values
nSim <- 3
nOM <- 2
nMP <- 4
nPI <- 4

values <- array(NA, dim=c(nSim, nOM, nMP, nPI))

pi_means <- runif(nPI, 5, 50)
for (om in 1:nOM) {
  for (mp in 1:nMP) {
    for (pi in 1:nPI) {
      values[, om, mp, pi] <- rlnorm(nSim,log(pi_means[pi]), 0.4)
    }
  }
}

# Create and populate Object
quilt <- Quilt(Code=c('PI1', 'PI2', 'PI3', 'PI4'),
               Label=c('Performance Indicator 1',
                       'Performance Indicator 2',
                       'Performance Indicator 3',
                       'Performance Indicator 4'),
               Description = c('This is the description for PI 1',
                               'This is the description for PI 2',
                               'This is the description for PI 3',
                               'This is the description for PI 4'),
               Value=values)




# Add to `Slick` object
slick <- Slick()
Quilt(slick) <- quilt

# Plots
plotQuilt(slick)
#> ℹ Note: `MPs` is empty. Using default MP names and colors

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"32.6\" data-max=\"51\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"44.1\" data-max=\"56.9\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"33.8\" data-max=\"78.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"52.2\" data-max=\"60.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[45.4,51,32.6,37.2],[50.2,56.9,49.6,44.1],[45.8,33.8,52,78.7],[52.2,58.9,60.1,56.7]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 32.6 ? \"#ADD8E680\" : value <= 34.44 ? \"#9DC4DD80\" : value <= 36.28 ? \"#8DB0D580\" : value <= 38.12 ? \"#7D9DCD80\" : value <= 39.96 ? \"#6E89C480\" : value <= 41.8 ? \"#5E75BC80\" : value <= 43.64 ? \"#4E62B480\" : value <= 45.48 ? \"#3E4EAC80\" : value <= 47.32 ? \"#2F3AA380\" : value <= 49.16 ? \"#1F279B80\" : value <= 51 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 44.1 ? \"#ADD8E680\" : value <= 45.38 ? \"#9DC4DD80\" : value <= 46.66 ? \"#8DB0D580\" : value <= 47.94 ? \"#7D9DCD80\" : value <= 49.22 ? \"#6E89C480\" : value <= 50.5 ? \"#5E75BC80\" : value <= 51.78 ? \"#4E62B480\" : value <= 53.06 ? \"#3E4EAC80\" : value <= 54.34 ? \"#2F3AA380\" : value <= 55.62 ? \"#1F279B80\" : value <= 56.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 33.8 ? \"#ADD8E680\" : value <= 38.29 ? \"#9DC4DD80\" : value <= 42.78 ? \"#8DB0D580\" : value <= 47.27 ? \"#7D9DCD80\" : value <= 51.76 ? \"#6E89C480\" : value <= 56.25 ? \"#5E75BC80\" : value <= 60.74 ? \"#4E62B480\" : value <= 65.23 ? \"#3E4EAC80\" : value <= 69.72 ? \"#2F3AA380\" : value <= 74.21 ? \"#1F279B80\" : value <= 78.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 52.2 ? \"#ADD8E680\" : value <= 52.99 ? \"#9DC4DD80\" : value <= 53.78 ? \"#8DB0D580\" : value <= 54.57 ? \"#7D9DCD80\" : value <= 55.36 ? \"#6E89C480\" : value <= 56.15 ? \"#5E75BC80\" : value <= 56.94 ? \"#4E62B480\" : value <= 57.73 ? \"#3E4EAC80\" : value <= 58.52 ? \"#2F3AA380\" : value <= 59.31 ? \"#1F279B80\" : value <= 60.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 45.4 50.2 45.8 52.2
#> [2,] 51.0 56.9 33.8 58.9
#> [3,] 32.6 49.6 52.0 60.1
#> [4,] 37.2 44.1 78.7 56.7

```

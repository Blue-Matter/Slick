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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"11.3\" data-max=\"21.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"26.6\" data-max=\"42.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"23.5\" data-max=\"75.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"18.3\" data-max=\"22.2\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[21.1,11.3,15.2,13.9],[42.6,26.6,29.8,27.3],[47.3,32.8,75.7,23.5],[21.5,21.5,18.3,22.2]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 11.3 ? \"#ADD8E680\" : value <= 12.28 ? \"#9DC4DD80\" : value <= 13.26 ? \"#8DB0D580\" : value <= 14.24 ? \"#7D9DCD80\" : value <= 15.22 ? \"#6E89C480\" : value <= 16.2 ? \"#5E75BC80\" : value <= 17.18 ? \"#4E62B480\" : value <= 18.16 ? \"#3E4EAC80\" : value <= 19.14 ? \"#2F3AA380\" : value <= 20.12 ? \"#1F279B80\" : value <= 21.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 26.6 ? \"#ADD8E680\" : value <= 28.2 ? \"#9DC4DD80\" : value <= 29.8 ? \"#8DB0D580\" : value <= 31.4 ? \"#7D9DCD80\" : value <= 33 ? \"#6E89C480\" : value <= 34.6 ? \"#5E75BC80\" : value <= 36.2 ? \"#4E62B480\" : value <= 37.8 ? \"#3E4EAC80\" : value <= 39.4 ? \"#2F3AA380\" : value <= 41 ? \"#1F279B80\" : value <= 42.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 23.5 ? \"#ADD8E680\" : value <= 28.72 ? \"#9DC4DD80\" : value <= 33.94 ? \"#8DB0D580\" : value <= 39.16 ? \"#7D9DCD80\" : value <= 44.38 ? \"#6E89C480\" : value <= 49.6 ? \"#5E75BC80\" : value <= 54.82 ? \"#4E62B480\" : value <= 60.04 ? \"#3E4EAC80\" : value <= 65.26 ? \"#2F3AA380\" : value <= 70.48 ? \"#1F279B80\" : value <= 75.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 18.3 ? \"#ADD8E680\" : value <= 18.69 ? \"#9DC4DD80\" : value <= 19.08 ? \"#8DB0D580\" : value <= 19.47 ? \"#7D9DCD80\" : value <= 19.86 ? \"#6E89C480\" : value <= 20.25 ? \"#5E75BC80\" : value <= 20.64 ? \"#4E62B480\" : value <= 21.03 ? \"#3E4EAC80\" : value <= 21.42 ? \"#2F3AA380\" : value <= 21.81 ? \"#1F279B80\" : value <= 22.2 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"52.4\" data-max=\"63.5\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"5.79\" data-max=\"9.23\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"15.5\" data-max=\"25.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"36.2\" data-max=\"64.9\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[63.5,52.4,55.8,58.6],[9.220000000000001,7.32,5.79,8.23],[20.2,25.8,19.9,15.5],[39.2,36.2,40.6,64.90000000000001]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 52.4 ? \"#ADD8E680\" : value <= 53.51 ? \"#9DC4DD80\" : value <= 54.62 ? \"#8DB0D580\" : value <= 55.73 ? \"#7D9DCD80\" : value <= 56.84 ? \"#6E89C480\" : value <= 57.95 ? \"#5E75BC80\" : value <= 59.06 ? \"#4E62B480\" : value <= 60.17 ? \"#3E4EAC80\" : value <= 61.28 ? \"#2F3AA380\" : value <= 62.39 ? \"#1F279B80\" : value <= 63.5 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 5.79 ? \"#ADD8E680\" : value <= 6.133 ? \"#9DC4DD80\" : value <= 6.476 ? \"#8DB0D580\" : value <= 6.819 ? \"#7D9DCD80\" : value <= 7.162 ? \"#6E89C480\" : value <= 7.505 ? \"#5E75BC80\" : value <= 7.848 ? \"#4E62B480\" : value <= 8.191 ? \"#3E4EAC80\" : value <= 8.534 ? \"#2F3AA380\" : value <= 8.877 ? \"#1F279B80\" : value <= 9.22 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 15.5 ? \"#ADD8E680\" : value <= 16.53 ? \"#9DC4DD80\" : value <= 17.56 ? \"#8DB0D580\" : value <= 18.59 ? \"#7D9DCD80\" : value <= 19.62 ? \"#6E89C480\" : value <= 20.65 ? \"#5E75BC80\" : value <= 21.68 ? \"#4E62B480\" : value <= 22.71 ? \"#3E4EAC80\" : value <= 23.74 ? \"#2F3AA380\" : value <= 24.77 ? \"#1F279B80\" : value <= 25.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 36.2 ? \"#ADD8E680\" : value <= 39.07 ? \"#9DC4DD80\" : value <= 41.94 ? \"#8DB0D580\" : value <= 44.81 ? \"#7D9DCD80\" : value <= 47.68 ? \"#6E89C480\" : value <= 50.55 ? \"#5E75BC80\" : value <= 53.42 ? \"#4E62B480\" : value <= 56.29 ? \"#3E4EAC80\" : value <= 59.16 ? \"#2F3AA380\" : value <= 62.03 ? \"#1F279B80\" : value <= 64.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 63.5  9.2 20.2 39.2
#> [2,] 52.4  7.3 25.8 36.2
#> [3,] 55.8  5.8 19.9 40.6
#> [4,] 58.6  8.2 15.5 64.9

```

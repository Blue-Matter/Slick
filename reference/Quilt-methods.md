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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"28.9\" data-max=\"47.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"41.2\" data-max=\"73.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"14.8\" data-max=\"46.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"13.7\" data-max=\"27.3\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[28.9,47.6,40.1,30.7],[41.2,50.2,73.40000000000001,53.5],[14.8,29.4,16.6,46.7],[18.4,27.3,25,13.7]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 28.9 ? \"#ADD8E680\" : value <= 30.77 ? \"#9DC4DD80\" : value <= 32.64 ? \"#8DB0D580\" : value <= 34.51 ? \"#7D9DCD80\" : value <= 36.38 ? \"#6E89C480\" : value <= 38.25 ? \"#5E75BC80\" : value <= 40.12 ? \"#4E62B480\" : value <= 41.99 ? \"#3E4EAC80\" : value <= 43.86 ? \"#2F3AA380\" : value <= 45.73 ? \"#1F279B80\" : value <= 47.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 41.2 ? \"#ADD8E680\" : value <= 44.42 ? \"#9DC4DD80\" : value <= 47.64 ? \"#8DB0D580\" : value <= 50.86 ? \"#7D9DCD80\" : value <= 54.08 ? \"#6E89C480\" : value <= 57.3 ? \"#5E75BC80\" : value <= 60.52 ? \"#4E62B480\" : value <= 63.74 ? \"#3E4EAC80\" : value <= 66.96 ? \"#2F3AA380\" : value <= 70.18 ? \"#1F279B80\" : value <= 73.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 14.8 ? \"#ADD8E680\" : value <= 17.99 ? \"#9DC4DD80\" : value <= 21.18 ? \"#8DB0D580\" : value <= 24.37 ? \"#7D9DCD80\" : value <= 27.56 ? \"#6E89C480\" : value <= 30.75 ? \"#5E75BC80\" : value <= 33.94 ? \"#4E62B480\" : value <= 37.13 ? \"#3E4EAC80\" : value <= 40.32 ? \"#2F3AA380\" : value <= 43.51 ? \"#1F279B80\" : value <= 46.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 13.7 ? \"#ADD8E680\" : value <= 15.06 ? \"#9DC4DD80\" : value <= 16.42 ? \"#8DB0D580\" : value <= 17.78 ? \"#7D9DCD80\" : value <= 19.14 ? \"#6E89C480\" : value <= 20.5 ? \"#5E75BC80\" : value <= 21.86 ? \"#4E62B480\" : value <= 23.22 ? \"#3E4EAC80\" : value <= 24.58 ? \"#2F3AA380\" : value <= 25.94 ? \"#1F279B80\" : value <= 27.3 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"19.8\" data-max=\"22.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"6.61\" data-max=\"9.4\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"19.8\" data-max=\"33\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"23.6\" data-max=\"42.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[19.8,21.1,22.1,21.1],[8.35,6.61,9.4,8.57],[33,25.5,19.8,24.6],[23.6,26.5,42.4,33.3]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 19.8 ? \"#ADD8E680\" : value <= 20.03 ? \"#9DC4DD80\" : value <= 20.26 ? \"#8DB0D580\" : value <= 20.49 ? \"#7D9DCD80\" : value <= 20.72 ? \"#6E89C480\" : value <= 20.95 ? \"#5E75BC80\" : value <= 21.18 ? \"#4E62B480\" : value <= 21.41 ? \"#3E4EAC80\" : value <= 21.64 ? \"#2F3AA380\" : value <= 21.87 ? \"#1F279B80\" : value <= 22.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 6.61 ? \"#ADD8E680\" : value <= 6.889 ? \"#9DC4DD80\" : value <= 7.168 ? \"#8DB0D580\" : value <= 7.447 ? \"#7D9DCD80\" : value <= 7.726 ? \"#6E89C480\" : value <= 8.005 ? \"#5E75BC80\" : value <= 8.284 ? \"#4E62B480\" : value <= 8.563 ? \"#3E4EAC80\" : value <= 8.842 ? \"#2F3AA380\" : value <= 9.121 ? \"#1F279B80\" : value <= 9.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 19.8 ? \"#ADD8E680\" : value <= 21.12 ? \"#9DC4DD80\" : value <= 22.44 ? \"#8DB0D580\" : value <= 23.76 ? \"#7D9DCD80\" : value <= 25.08 ? \"#6E89C480\" : value <= 26.4 ? \"#5E75BC80\" : value <= 27.72 ? \"#4E62B480\" : value <= 29.04 ? \"#3E4EAC80\" : value <= 30.36 ? \"#2F3AA380\" : value <= 31.68 ? \"#1F279B80\" : value <= 33 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 23.6 ? \"#ADD8E680\" : value <= 25.48 ? \"#9DC4DD80\" : value <= 27.36 ? \"#8DB0D580\" : value <= 29.24 ? \"#7D9DCD80\" : value <= 31.12 ? \"#6E89C480\" : value <= 33 ? \"#5E75BC80\" : value <= 34.88 ? \"#4E62B480\" : value <= 36.76 ? \"#3E4EAC80\" : value <= 38.64 ? \"#2F3AA380\" : value <= 40.52 ? \"#1F279B80\" : value <= 42.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 19.8  8.4 33.0 23.6
#> [2,] 21.1  6.6 25.5 26.5
#> [3,] 22.1  9.4 19.8 42.4
#> [4,] 21.1  8.6 24.6 33.3

```

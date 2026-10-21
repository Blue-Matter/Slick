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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"23.5\" data-max=\"38.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"7.1\" data-max=\"12.7\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"12.9\" data-max=\"41.5\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"15.5\" data-max=\"27.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[25.7,23.5,38.8,32.7],[7.75,7.1,8.65,12.7],[41.5,12.9,25.6,14.5],[15.5,18.7,27.7,25.3]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 23.5 ? \"#ADD8E680\" : value <= 25.03 ? \"#9DC4DD80\" : value <= 26.56 ? \"#8DB0D580\" : value <= 28.09 ? \"#7D9DCD80\" : value <= 29.62 ? \"#6E89C480\" : value <= 31.15 ? \"#5E75BC80\" : value <= 32.68 ? \"#4E62B480\" : value <= 34.21 ? \"#3E4EAC80\" : value <= 35.74 ? \"#2F3AA380\" : value <= 37.27 ? \"#1F279B80\" : value <= 38.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 7.1 ? \"#ADD8E680\" : value <= 7.66 ? \"#9DC4DD80\" : value <= 8.22 ? \"#8DB0D580\" : value <= 8.78 ? \"#7D9DCD80\" : value <= 9.34 ? \"#6E89C480\" : value <= 9.9 ? \"#5E75BC80\" : value <= 10.46 ? \"#4E62B480\" : value <= 11.02 ? \"#3E4EAC80\" : value <= 11.58 ? \"#2F3AA380\" : value <= 12.14 ? \"#1F279B80\" : value <= 12.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 12.9 ? \"#ADD8E680\" : value <= 15.76 ? \"#9DC4DD80\" : value <= 18.62 ? \"#8DB0D580\" : value <= 21.48 ? \"#7D9DCD80\" : value <= 24.34 ? \"#6E89C480\" : value <= 27.2 ? \"#5E75BC80\" : value <= 30.06 ? \"#4E62B480\" : value <= 32.92 ? \"#3E4EAC80\" : value <= 35.78 ? \"#2F3AA380\" : value <= 38.64 ? \"#1F279B80\" : value <= 41.5 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 15.5 ? \"#ADD8E680\" : value <= 16.72 ? \"#9DC4DD80\" : value <= 17.94 ? \"#8DB0D580\" : value <= 19.16 ? \"#7D9DCD80\" : value <= 20.38 ? \"#6E89C480\" : value <= 21.6 ? \"#5E75BC80\" : value <= 22.82 ? \"#4E62B480\" : value <= 24.04 ? \"#3E4EAC80\" : value <= 25.26 ? \"#2F3AA380\" : value <= 26.48 ? \"#1F279B80\" : value <= 27.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"21.6\" data-max=\"27.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"9.28\" data-max=\"11.9\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"36.5\" data-max=\"54.5\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"41.4\" data-max=\"53.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[23.2,27.4,21.6,27.8],[9.289999999999999,10.4,11.1,11.9],[50.2,36.5,54.5,49.9],[53.7,44.7,41.4,45.1]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 21.6 ? \"#ADD8E680\" : value <= 22.22 ? \"#9DC4DD80\" : value <= 22.84 ? \"#8DB0D580\" : value <= 23.46 ? \"#7D9DCD80\" : value <= 24.08 ? \"#6E89C480\" : value <= 24.7 ? \"#5E75BC80\" : value <= 25.32 ? \"#4E62B480\" : value <= 25.94 ? \"#3E4EAC80\" : value <= 26.56 ? \"#2F3AA380\" : value <= 27.18 ? \"#1F279B80\" : value <= 27.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 9.29 ? \"#ADD8E680\" : value <= 9.551 ? \"#9DC4DD80\" : value <= 9.812 ? \"#8DB0D580\" : value <= 10.073 ? \"#7D9DCD80\" : value <= 10.334 ? \"#6E89C480\" : value <= 10.595 ? \"#5E75BC80\" : value <= 10.856 ? \"#4E62B480\" : value <= 11.117 ? \"#3E4EAC80\" : value <= 11.378 ? \"#2F3AA380\" : value <= 11.639 ? \"#1F279B80\" : value <= 11.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 36.5 ? \"#ADD8E680\" : value <= 38.3 ? \"#9DC4DD80\" : value <= 40.1 ? \"#8DB0D580\" : value <= 41.9 ? \"#7D9DCD80\" : value <= 43.7 ? \"#6E89C480\" : value <= 45.5 ? \"#5E75BC80\" : value <= 47.3 ? \"#4E62B480\" : value <= 49.1 ? \"#3E4EAC80\" : value <= 50.9 ? \"#2F3AA380\" : value <= 52.7 ? \"#1F279B80\" : value <= 54.5 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 41.4 ? \"#ADD8E680\" : value <= 42.63 ? \"#9DC4DD80\" : value <= 43.86 ? \"#8DB0D580\" : value <= 45.09 ? \"#7D9DCD80\" : value <= 46.32 ? \"#6E89C480\" : value <= 47.55 ? \"#5E75BC80\" : value <= 48.78 ? \"#4E62B480\" : value <= 50.01 ? \"#3E4EAC80\" : value <= 51.24 ? \"#2F3AA380\" : value <= 52.47 ? \"#1F279B80\" : value <= 53.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 23.2  9.3 50.2 53.7
#> [2,] 27.4 10.4 36.5 44.7
#> [3,] 21.6 11.1 54.5 41.4
#> [4,] 27.8 11.9 49.9 45.1



# Example `Slick` object included with the package (see `?WSKJ`)
plotQuilt(WSKJ)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.695\" data-max=\"0.714\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.732\" data-max=\"0.759\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.736\" data-max=\"0.763\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"30000\" data-max=\"31900\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["CE","IR","SP","SPAH"],[0.708,0.714,0.707,0.695],[0.759,0.756,0.748,0.732],[0.759,0.763,0.752,0.736],[30700,30900,30000,31900]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PGK<\/th>\n      <th>P100<\/th>\n      <th>PNOF<\/th>\n      <th>Mean TAC<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PGK","targets":1},{"name":"P100","targets":2},{"name":"PNOF","targets":3},{"name":"Mean TAC","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.695 ? \"#ADD8E680\" : value <= 0.6969 ? \"#9DC4DD80\" : value <= 0.6988 ? \"#8DB0D580\" : value <= 0.7007 ? \"#7D9DCD80\" : value <= 0.7026 ? \"#6E89C480\" : value <= 0.7045 ? \"#5E75BC80\" : value <= 0.7064 ? \"#4E62B480\" : value <= 0.7083 ? \"#3E4EAC80\" : value <= 0.7102 ? \"#2F3AA380\" : value <= 0.7121 ? \"#1F279B80\" : value <= 0.714 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.732 ? \"#ADD8E680\" : value <= 0.7347 ? \"#9DC4DD80\" : value <= 0.7374 ? \"#8DB0D580\" : value <= 0.7401 ? \"#7D9DCD80\" : value <= 0.7428 ? \"#6E89C480\" : value <= 0.7455 ? \"#5E75BC80\" : value <= 0.7482 ? \"#4E62B480\" : value <= 0.7509 ? \"#3E4EAC80\" : value <= 0.7536 ? \"#2F3AA380\" : value <= 0.7563 ? \"#1F279B80\" : value <= 0.759 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.736 ? \"#ADD8E680\" : value <= 0.7387 ? \"#9DC4DD80\" : value <= 0.7414 ? \"#8DB0D580\" : value <= 0.7441 ? \"#7D9DCD80\" : value <= 0.7468 ? \"#6E89C480\" : value <= 0.7495 ? \"#5E75BC80\" : value <= 0.7522 ? \"#4E62B480\" : value <= 0.7549 ? \"#3E4EAC80\" : value <= 0.7576 ? \"#2F3AA380\" : value <= 0.7603 ? \"#1F279B80\" : value <= 0.763 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 30000 ? \"#ADD8E680\" : value <= 30190 ? \"#9DC4DD80\" : value <= 30380 ? \"#8DB0D580\" : value <= 30570 ? \"#7D9DCD80\" : value <= 30760 ? \"#6E89C480\" : value <= 30950 ? \"#5E75BC80\" : value <= 31140 ? \"#4E62B480\" : value <= 31330 ? \"#3E4EAC80\" : value <= 31520 ? \"#2F3AA380\" : value <= 31710 ? \"#1F279B80\" : value <= 31900 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}
```

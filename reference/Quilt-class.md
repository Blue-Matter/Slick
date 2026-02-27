# S4 class `Quilt`

Objects of class `Quilt` are used to store information for the Quilt
chart. Like all S4 objects in `Slick`, slots in this object can be
accessed and assigned using functions corresponding to slot name. See
[`Quilt()`](https://slick.bluematterscience.com/reference/Quilt-methods.md)
and the the `See Also` section below.

## Details

Objects of class `Quilt` are created with
[`Quilt()`](https://slick.bluematterscience.com/reference/Quilt-methods.md)

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

## Slots

- `Code`:

  A *short* code for the Performance Indicators for this object. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- `Label`:

  A short label for the Performance Indicators for this object. Used to
  label axes on charts. Can be longer than `Code` but recommended to
  keep short as possible so it shows clearly in plots and tables. A
  character string length `nPI` or a named list for multi-language
  support. See `Details`

- `Description`:

  A description for the Performance Indicators for this object. Can
  include Markdown, see `Examples`. A character string length `nPI` or a
  named list for multi-language support. See `Details`

- `Value`:

  A numeric array with the stochastic performance indicator values for
  each operating model (OM), management procedure (MP), and performance
  indicator (PI). Dimensions: c(`nOM`, `nMP`, and `nPI`). Alternatively,
  to calculate average over both simulations and Operating Models,
  `Value` can be a 4-dimensional array with dimensions: c(`nSim`, `nOM`,
  `nMP`, and `nPI`).

- `Preset`:

  An optional named list for the preset buttons in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md). The
  name of the list element will appear as a button in the
  [`App()`](https://slick.bluematterscience.com/reference/App.md).

- `Color`:

  A character vector length 2 of colors for the maximum and minimum
  values in the chart.

- `MinValue`:

  Numeric vector length `nPI` with the minimum possible value for the
  respective PIs. Defaults to minimum PI value in `Value` (averaged
  across OMs in some cases)

- `MaxValue`:

  Numeric vector length `nPI` with the maximum possible value (i.e.,
  best performance) for the respective PIs. Defaults to maximum PI value
  in `Value` (averaged across OMs in some cases).

- `Misc`:

  A named list for additional miscellaneous information.

## See also

[`Quilt()`](https://slick.bluematterscience.com/reference/Quilt-methods.md),
[`Code()`](https://slick.bluematterscience.com/reference/Code.md),
[`Label()`](https://slick.bluematterscience.com/reference/Code.md),
[`Description()`](https://slick.bluematterscience.com/reference/Code.md),
[`Value()`](https://slick.bluematterscience.com/reference/Value.md),
[`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"23.2\" data-max=\"41.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"25.2\" data-max=\"55.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"6.18\" data-max=\"8.26\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"16.5\" data-max=\"29.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[23.2,41.2,38.2,41.7],[55.4,25.2,33.6,37.4],[7.3,6.23,6.18,8.26],[19.3,16.5,28.3,29.8]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 23.2 ? \"#ADD8E680\" : value <= 25.05 ? \"#9DC4DD80\" : value <= 26.9 ? \"#8DB0D580\" : value <= 28.75 ? \"#7D9DCD80\" : value <= 30.6 ? \"#6E89C480\" : value <= 32.45 ? \"#5E75BC80\" : value <= 34.3 ? \"#4E62B480\" : value <= 36.15 ? \"#3E4EAC80\" : value <= 38 ? \"#2F3AA380\" : value <= 39.85 ? \"#1F279B80\" : value <= 41.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 25.2 ? \"#ADD8E680\" : value <= 28.22 ? \"#9DC4DD80\" : value <= 31.24 ? \"#8DB0D580\" : value <= 34.26 ? \"#7D9DCD80\" : value <= 37.28 ? \"#6E89C480\" : value <= 40.3 ? \"#5E75BC80\" : value <= 43.32 ? \"#4E62B480\" : value <= 46.34 ? \"#3E4EAC80\" : value <= 49.36 ? \"#2F3AA380\" : value <= 52.38 ? \"#1F279B80\" : value <= 55.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 6.18 ? \"#ADD8E680\" : value <= 6.388 ? \"#9DC4DD80\" : value <= 6.596 ? \"#8DB0D580\" : value <= 6.804 ? \"#7D9DCD80\" : value <= 7.012 ? \"#6E89C480\" : value <= 7.22 ? \"#5E75BC80\" : value <= 7.428 ? \"#4E62B480\" : value <= 7.636 ? \"#3E4EAC80\" : value <= 7.844 ? \"#2F3AA380\" : value <= 8.052 ? \"#1F279B80\" : value <= 8.26 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 16.5 ? \"#ADD8E680\" : value <= 17.83 ? \"#9DC4DD80\" : value <= 19.16 ? \"#8DB0D580\" : value <= 20.49 ? \"#7D9DCD80\" : value <= 21.82 ? \"#6E89C480\" : value <= 23.15 ? \"#5E75BC80\" : value <= 24.48 ? \"#4E62B480\" : value <= 25.81 ? \"#3E4EAC80\" : value <= 27.14 ? \"#2F3AA380\" : value <= 28.47 ? \"#1F279B80\" : value <= 29.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"27.9\" data-max=\"33.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"41.6\" data-max=\"54.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"6.59\" data-max=\"11.4\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"36.7\" data-max=\"52.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[33.7,27.9,31.8,31.2],[42,48.3,41.6,54.1],[11,9.57,11.4,6.59],[52.6,52.3,45.8,36.7]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 27.9 ? \"#ADD8E680\" : value <= 28.48 ? \"#9DC4DD80\" : value <= 29.06 ? \"#8DB0D580\" : value <= 29.64 ? \"#7D9DCD80\" : value <= 30.22 ? \"#6E89C480\" : value <= 30.8 ? \"#5E75BC80\" : value <= 31.38 ? \"#4E62B480\" : value <= 31.96 ? \"#3E4EAC80\" : value <= 32.54 ? \"#2F3AA380\" : value <= 33.12 ? \"#1F279B80\" : value <= 33.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 41.6 ? \"#ADD8E680\" : value <= 42.85 ? \"#9DC4DD80\" : value <= 44.1 ? \"#8DB0D580\" : value <= 45.35 ? \"#7D9DCD80\" : value <= 46.6 ? \"#6E89C480\" : value <= 47.85 ? \"#5E75BC80\" : value <= 49.1 ? \"#4E62B480\" : value <= 50.35 ? \"#3E4EAC80\" : value <= 51.6 ? \"#2F3AA380\" : value <= 52.85 ? \"#1F279B80\" : value <= 54.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 6.59 ? \"#ADD8E680\" : value <= 7.071 ? \"#9DC4DD80\" : value <= 7.552 ? \"#8DB0D580\" : value <= 8.033 ? \"#7D9DCD80\" : value <= 8.514 ? \"#6E89C480\" : value <= 8.995 ? \"#5E75BC80\" : value <= 9.476 ? \"#4E62B480\" : value <= 9.957 ? \"#3E4EAC80\" : value <= 10.438 ? \"#2F3AA380\" : value <= 10.919 ? \"#1F279B80\" : value <= 11.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 36.7 ? \"#ADD8E680\" : value <= 38.29 ? \"#9DC4DD80\" : value <= 39.88 ? \"#8DB0D580\" : value <= 41.47 ? \"#7D9DCD80\" : value <= 43.06 ? \"#6E89C480\" : value <= 44.65 ? \"#5E75BC80\" : value <= 46.24 ? \"#4E62B480\" : value <= 47.83 ? \"#3E4EAC80\" : value <= 49.42 ? \"#2F3AA380\" : value <= 51.01 ? \"#1F279B80\" : value <= 52.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 33.7 42.0 11.0 52.6
#> [2,] 27.9 48.3  9.6 52.3
#> [3,] 31.8 41.6 11.4 45.8
#> [4,] 31.2 54.1  6.6 36.7

```

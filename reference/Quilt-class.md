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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"29.3\" data-max=\"91.5\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"28\" data-max=\"52.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"14\" data-max=\"26.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"8.15\" data-max=\"14.5\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[29.3,37.4,47.6,91.5],[28.5,28,52.7,35.2],[14,20.3,26.8,18.3],[11,9.65,8.15,14.5]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 29.3 ? \"#ADD8E680\" : value <= 35.52 ? \"#9DC4DD80\" : value <= 41.74 ? \"#8DB0D580\" : value <= 47.96 ? \"#7D9DCD80\" : value <= 54.18 ? \"#6E89C480\" : value <= 60.4 ? \"#5E75BC80\" : value <= 66.62 ? \"#4E62B480\" : value <= 72.84 ? \"#3E4EAC80\" : value <= 79.06 ? \"#2F3AA380\" : value <= 85.28 ? \"#1F279B80\" : value <= 91.5 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 28 ? \"#ADD8E680\" : value <= 30.47 ? \"#9DC4DD80\" : value <= 32.94 ? \"#8DB0D580\" : value <= 35.41 ? \"#7D9DCD80\" : value <= 37.88 ? \"#6E89C480\" : value <= 40.35 ? \"#5E75BC80\" : value <= 42.82 ? \"#4E62B480\" : value <= 45.29 ? \"#3E4EAC80\" : value <= 47.76 ? \"#2F3AA380\" : value <= 50.23 ? \"#1F279B80\" : value <= 52.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 14 ? \"#ADD8E680\" : value <= 15.28 ? \"#9DC4DD80\" : value <= 16.56 ? \"#8DB0D580\" : value <= 17.84 ? \"#7D9DCD80\" : value <= 19.12 ? \"#6E89C480\" : value <= 20.4 ? \"#5E75BC80\" : value <= 21.68 ? \"#4E62B480\" : value <= 22.96 ? \"#3E4EAC80\" : value <= 24.24 ? \"#2F3AA380\" : value <= 25.52 ? \"#1F279B80\" : value <= 26.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 8.15 ? \"#ADD8E680\" : value <= 8.785 ? \"#9DC4DD80\" : value <= 9.42 ? \"#8DB0D580\" : value <= 10.055 ? \"#7D9DCD80\" : value <= 10.69 ? \"#6E89C480\" : value <= 11.325 ? \"#5E75BC80\" : value <= 11.96 ? \"#4E62B480\" : value <= 12.595 ? \"#3E4EAC80\" : value <= 13.23 ? \"#2F3AA380\" : value <= 13.865 ? \"#1F279B80\" : value <= 14.5 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"14.8\" data-max=\"18.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"22.8\" data-max=\"27.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"13.4\" data-max=\"21.2\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"24.8\" data-max=\"38\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[16.1,18.8,14.8,16.8],[26.1,22.8,27.6,23.7],[13.4,20.7,16.8,21.2],[24.8,32.3,38,25.7]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 14.8 ? \"#ADD8E680\" : value <= 15.2 ? \"#9DC4DD80\" : value <= 15.6 ? \"#8DB0D580\" : value <= 16 ? \"#7D9DCD80\" : value <= 16.4 ? \"#6E89C480\" : value <= 16.8 ? \"#5E75BC80\" : value <= 17.2 ? \"#4E62B480\" : value <= 17.6 ? \"#3E4EAC80\" : value <= 18 ? \"#2F3AA380\" : value <= 18.4 ? \"#1F279B80\" : value <= 18.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 22.8 ? \"#ADD8E680\" : value <= 23.28 ? \"#9DC4DD80\" : value <= 23.76 ? \"#8DB0D580\" : value <= 24.24 ? \"#7D9DCD80\" : value <= 24.72 ? \"#6E89C480\" : value <= 25.2 ? \"#5E75BC80\" : value <= 25.68 ? \"#4E62B480\" : value <= 26.16 ? \"#3E4EAC80\" : value <= 26.64 ? \"#2F3AA380\" : value <= 27.12 ? \"#1F279B80\" : value <= 27.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 13.4 ? \"#ADD8E680\" : value <= 14.18 ? \"#9DC4DD80\" : value <= 14.96 ? \"#8DB0D580\" : value <= 15.74 ? \"#7D9DCD80\" : value <= 16.52 ? \"#6E89C480\" : value <= 17.3 ? \"#5E75BC80\" : value <= 18.08 ? \"#4E62B480\" : value <= 18.86 ? \"#3E4EAC80\" : value <= 19.64 ? \"#2F3AA380\" : value <= 20.42 ? \"#1F279B80\" : value <= 21.2 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 24.8 ? \"#ADD8E680\" : value <= 26.12 ? \"#9DC4DD80\" : value <= 27.44 ? \"#8DB0D580\" : value <= 28.76 ? \"#7D9DCD80\" : value <= 30.08 ? \"#6E89C480\" : value <= 31.4 ? \"#5E75BC80\" : value <= 32.72 ? \"#4E62B480\" : value <= 34.04 ? \"#3E4EAC80\" : value <= 35.36 ? \"#2F3AA380\" : value <= 36.68 ? \"#1F279B80\" : value <= 38 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 16.1 26.1 13.4 24.8
#> [2,] 18.8 22.8 20.7 32.3
#> [3,] 14.8 27.6 16.8 38.0
#> [4,] 16.8 23.7 21.2 25.7

```

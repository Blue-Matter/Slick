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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"13.9\" data-max=\"30.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"24.9\" data-max=\"44.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"9.21\" data-max=\"15.8\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"6.45\" data-max=\"11.6\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[16,30.7,13.9,18.6],[44.1,29.5,25.1,24.9],[15.7,10.7,9.210000000000001,15.8],[6.45,11.5,10.6,11.6]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 13.9 ? \"#ADD8E680\" : value <= 15.58 ? \"#9DC4DD80\" : value <= 17.26 ? \"#8DB0D580\" : value <= 18.94 ? \"#7D9DCD80\" : value <= 20.62 ? \"#6E89C480\" : value <= 22.3 ? \"#5E75BC80\" : value <= 23.98 ? \"#4E62B480\" : value <= 25.66 ? \"#3E4EAC80\" : value <= 27.34 ? \"#2F3AA380\" : value <= 29.02 ? \"#1F279B80\" : value <= 30.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 24.9 ? \"#ADD8E680\" : value <= 26.82 ? \"#9DC4DD80\" : value <= 28.74 ? \"#8DB0D580\" : value <= 30.66 ? \"#7D9DCD80\" : value <= 32.58 ? \"#6E89C480\" : value <= 34.5 ? \"#5E75BC80\" : value <= 36.42 ? \"#4E62B480\" : value <= 38.34 ? \"#3E4EAC80\" : value <= 40.26 ? \"#2F3AA380\" : value <= 42.18 ? \"#1F279B80\" : value <= 44.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 9.21 ? \"#ADD8E680\" : value <= 9.869 ? \"#9DC4DD80\" : value <= 10.528 ? \"#8DB0D580\" : value <= 11.187 ? \"#7D9DCD80\" : value <= 11.846 ? \"#6E89C480\" : value <= 12.505 ? \"#5E75BC80\" : value <= 13.164 ? \"#4E62B480\" : value <= 13.823 ? \"#3E4EAC80\" : value <= 14.482 ? \"#2F3AA380\" : value <= 15.141 ? \"#1F279B80\" : value <= 15.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 6.45 ? \"#ADD8E680\" : value <= 6.965 ? \"#9DC4DD80\" : value <= 7.48 ? \"#8DB0D580\" : value <= 7.995 ? \"#7D9DCD80\" : value <= 8.51 ? \"#6E89C480\" : value <= 9.025 ? \"#5E75BC80\" : value <= 9.54 ? \"#4E62B480\" : value <= 10.055 ? \"#3E4EAC80\" : value <= 10.57 ? \"#2F3AA380\" : value <= 11.085 ? \"#1F279B80\" : value <= 11.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"17.9\" data-max=\"29.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"36\" data-max=\"43.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"20.9\" data-max=\"27.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"26.9\" data-max=\"46.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[17.9,29.4,29.2,25.6],[43.4,36,40.9,40.2],[21.1,24.2,20.9,27.1],[45.1,39.1,46.4,26.9]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 17.9 ? \"#ADD8E680\" : value <= 19.05 ? \"#9DC4DD80\" : value <= 20.2 ? \"#8DB0D580\" : value <= 21.35 ? \"#7D9DCD80\" : value <= 22.5 ? \"#6E89C480\" : value <= 23.65 ? \"#5E75BC80\" : value <= 24.8 ? \"#4E62B480\" : value <= 25.95 ? \"#3E4EAC80\" : value <= 27.1 ? \"#2F3AA380\" : value <= 28.25 ? \"#1F279B80\" : value <= 29.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 36 ? \"#ADD8E680\" : value <= 36.74 ? \"#9DC4DD80\" : value <= 37.48 ? \"#8DB0D580\" : value <= 38.22 ? \"#7D9DCD80\" : value <= 38.96 ? \"#6E89C480\" : value <= 39.7 ? \"#5E75BC80\" : value <= 40.44 ? \"#4E62B480\" : value <= 41.18 ? \"#3E4EAC80\" : value <= 41.92 ? \"#2F3AA380\" : value <= 42.66 ? \"#1F279B80\" : value <= 43.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 20.9 ? \"#ADD8E680\" : value <= 21.52 ? \"#9DC4DD80\" : value <= 22.14 ? \"#8DB0D580\" : value <= 22.76 ? \"#7D9DCD80\" : value <= 23.38 ? \"#6E89C480\" : value <= 24 ? \"#5E75BC80\" : value <= 24.62 ? \"#4E62B480\" : value <= 25.24 ? \"#3E4EAC80\" : value <= 25.86 ? \"#2F3AA380\" : value <= 26.48 ? \"#1F279B80\" : value <= 27.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 26.9 ? \"#ADD8E680\" : value <= 28.85 ? \"#9DC4DD80\" : value <= 30.8 ? \"#8DB0D580\" : value <= 32.75 ? \"#7D9DCD80\" : value <= 34.7 ? \"#6E89C480\" : value <= 36.65 ? \"#5E75BC80\" : value <= 38.6 ? \"#4E62B480\" : value <= 40.55 ? \"#3E4EAC80\" : value <= 42.5 ? \"#2F3AA380\" : value <= 44.45 ? \"#1F279B80\" : value <= 46.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 17.9 43.4 21.1 45.1
#> [2,] 29.4 36.0 24.2 39.1
#> [3,] 29.2 40.9 20.9 46.4
#> [4,] 25.6 40.2 27.1 26.9



# Example `Slick` object included with the package (see `?WSKJ`)
plotQuilt(WSKJ)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.695\" data-max=\"0.714\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.732\" data-max=\"0.759\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.736\" data-max=\"0.763\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"30000\" data-max=\"31900\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["CE","IR","SP","SPAH"],[0.708,0.714,0.707,0.695],[0.759,0.756,0.748,0.732],[0.759,0.763,0.752,0.736],[30700,30900,30000,31900]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PGK<\/th>\n      <th>P100<\/th>\n      <th>PNOF<\/th>\n      <th>Mean TAC<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PGK","targets":1},{"name":"P100","targets":2},{"name":"PNOF","targets":3},{"name":"Mean TAC","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.695 ? \"#ADD8E680\" : value <= 0.6969 ? \"#9DC4DD80\" : value <= 0.6988 ? \"#8DB0D580\" : value <= 0.7007 ? \"#7D9DCD80\" : value <= 0.7026 ? \"#6E89C480\" : value <= 0.7045 ? \"#5E75BC80\" : value <= 0.7064 ? \"#4E62B480\" : value <= 0.7083 ? \"#3E4EAC80\" : value <= 0.7102 ? \"#2F3AA380\" : value <= 0.7121 ? \"#1F279B80\" : value <= 0.714 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.732 ? \"#ADD8E680\" : value <= 0.7347 ? \"#9DC4DD80\" : value <= 0.7374 ? \"#8DB0D580\" : value <= 0.7401 ? \"#7D9DCD80\" : value <= 0.7428 ? \"#6E89C480\" : value <= 0.7455 ? \"#5E75BC80\" : value <= 0.7482 ? \"#4E62B480\" : value <= 0.7509 ? \"#3E4EAC80\" : value <= 0.7536 ? \"#2F3AA380\" : value <= 0.7563 ? \"#1F279B80\" : value <= 0.759 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.736 ? \"#ADD8E680\" : value <= 0.7387 ? \"#9DC4DD80\" : value <= 0.7414 ? \"#8DB0D580\" : value <= 0.7441 ? \"#7D9DCD80\" : value <= 0.7468 ? \"#6E89C480\" : value <= 0.7495 ? \"#5E75BC80\" : value <= 0.7522 ? \"#4E62B480\" : value <= 0.7549 ? \"#3E4EAC80\" : value <= 0.7576 ? \"#2F3AA380\" : value <= 0.7603 ? \"#1F279B80\" : value <= 0.763 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 30000 ? \"#ADD8E680\" : value <= 30190 ? \"#9DC4DD80\" : value <= 30380 ? \"#8DB0D580\" : value <= 30570 ? \"#7D9DCD80\" : value <= 30760 ? \"#6E89C480\" : value <= 30950 ? \"#5E75BC80\" : value <= 31140 ? \"#4E62B480\" : value <= 31330 ? \"#3E4EAC80\" : value <= 31520 ? \"#2F3AA380\" : value <= 31710 ? \"#1F279B80\" : value <= 31900 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}
```

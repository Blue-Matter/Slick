# Plot `Quilt`

Create a Quilt plot (unless `shading==FALSE` in which case it's just a
table)

## Usage

``` r
plotQuilt(
  slick,
  MP_label = "Code",
  OMs = NA,
  minmax = FALSE,
  shading = TRUE,
  kable = FALSE,
  signif = 3,
  alpha = 0.5
)
```

## Arguments

- slick:

  A
  [`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
  object

- MP_label:

  Label to use for the MPs. Either `Code` or `Label`. `Description`
  works as well, but you probably don't want to do that.

- OMs:

  Integers representing the OMs to include in the plot. Defaults to all.

- minmax:

  Logical. Color shading from min to max values in each column? If TRUE,
  ignores `MinValue(quilt)` and `MaxValue(quilt)`

- shading:

  Logical. Color shading for the columns?

- kable:

  Logical. Return a `kable` object?

- signif:

  Numeric Number of significant figures

- alpha:

  Numeric value. Transparency for color shading

## Value

A [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) or a
[`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html) object

## Details

The columns are color shaded from light (lowest values) to dark (highest
values).

Colors are set in `Color(quilt)`.

The color shading has 10 steps, from `MinValue(quilt)` to
`MaxValue(quilt)` for each Performance Indicator. If those values are
missing (`NA`) for a given PI, colors are shaded from lowest to highest
values. If `minmax==TRUE`, `MinValue(quilt)` and `MaxValue(quilt)` are
ignored.

## See also

[`Quilt()`](https://slick.bluematterscience.com/reference/Quilt-methods.md),
[`Quilt-class()`](https://slick.bluematterscience.com/reference/Quilt-class.md)

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"13.6\" data-max=\"23.9\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"16.5\" data-max=\"48.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"9.58\" data-max=\"12.7\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"5.51\" data-max=\"15\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[23.9,15.6,19.1,13.6],[48.4,16.5,40.2,47.8],[12.7,9.58,10.1,9.859999999999999],[12.1,7.3,15,5.51]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 13.6 ? \"#ADD8E680\" : value <= 14.63 ? \"#9DC4DD80\" : value <= 15.66 ? \"#8DB0D580\" : value <= 16.69 ? \"#7D9DCD80\" : value <= 17.72 ? \"#6E89C480\" : value <= 18.75 ? \"#5E75BC80\" : value <= 19.78 ? \"#4E62B480\" : value <= 20.81 ? \"#3E4EAC80\" : value <= 21.84 ? \"#2F3AA380\" : value <= 22.87 ? \"#1F279B80\" : value <= 23.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 16.5 ? \"#ADD8E680\" : value <= 19.69 ? \"#9DC4DD80\" : value <= 22.88 ? \"#8DB0D580\" : value <= 26.07 ? \"#7D9DCD80\" : value <= 29.26 ? \"#6E89C480\" : value <= 32.45 ? \"#5E75BC80\" : value <= 35.64 ? \"#4E62B480\" : value <= 38.83 ? \"#3E4EAC80\" : value <= 42.02 ? \"#2F3AA380\" : value <= 45.21 ? \"#1F279B80\" : value <= 48.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 9.58 ? \"#ADD8E680\" : value <= 9.892 ? \"#9DC4DD80\" : value <= 10.204 ? \"#8DB0D580\" : value <= 10.516 ? \"#7D9DCD80\" : value <= 10.828 ? \"#6E89C480\" : value <= 11.14 ? \"#5E75BC80\" : value <= 11.452 ? \"#4E62B480\" : value <= 11.764 ? \"#3E4EAC80\" : value <= 12.076 ? \"#2F3AA380\" : value <= 12.388 ? \"#1F279B80\" : value <= 12.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 5.51 ? \"#ADD8E680\" : value <= 6.459 ? \"#9DC4DD80\" : value <= 7.408 ? \"#8DB0D580\" : value <= 8.357 ? \"#7D9DCD80\" : value <= 9.306 ? \"#6E89C480\" : value <= 10.255 ? \"#5E75BC80\" : value <= 11.204 ? \"#4E62B480\" : value <= 12.153 ? \"#3E4EAC80\" : value <= 13.102 ? \"#2F3AA380\" : value <= 14.051 ? \"#1F279B80\" : value <= 15 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"28.7\" data-max=\"34.7\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"9.74\" data-max=\"13.4\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"35.3\" data-max=\"50.5\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"22.4\" data-max=\"38.3\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[28.7,32.9,31.6,34.7],[13.4,11.4,9.74,10.2],[50.5,35.3,39.4,43.1],[30.9,36.3,22.4,38.3]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 28.7 ? \"#ADD8E680\" : value <= 29.3 ? \"#9DC4DD80\" : value <= 29.9 ? \"#8DB0D580\" : value <= 30.5 ? \"#7D9DCD80\" : value <= 31.1 ? \"#6E89C480\" : value <= 31.7 ? \"#5E75BC80\" : value <= 32.3 ? \"#4E62B480\" : value <= 32.9 ? \"#3E4EAC80\" : value <= 33.5 ? \"#2F3AA380\" : value <= 34.1 ? \"#1F279B80\" : value <= 34.7 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 9.74 ? \"#ADD8E680\" : value <= 10.106 ? \"#9DC4DD80\" : value <= 10.472 ? \"#8DB0D580\" : value <= 10.838 ? \"#7D9DCD80\" : value <= 11.204 ? \"#6E89C480\" : value <= 11.57 ? \"#5E75BC80\" : value <= 11.936 ? \"#4E62B480\" : value <= 12.302 ? \"#3E4EAC80\" : value <= 12.668 ? \"#2F3AA380\" : value <= 13.034 ? \"#1F279B80\" : value <= 13.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 35.3 ? \"#ADD8E680\" : value <= 36.82 ? \"#9DC4DD80\" : value <= 38.34 ? \"#8DB0D580\" : value <= 39.86 ? \"#7D9DCD80\" : value <= 41.38 ? \"#6E89C480\" : value <= 42.9 ? \"#5E75BC80\" : value <= 44.42 ? \"#4E62B480\" : value <= 45.94 ? \"#3E4EAC80\" : value <= 47.46 ? \"#2F3AA380\" : value <= 48.98 ? \"#1F279B80\" : value <= 50.5 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 22.4 ? \"#ADD8E680\" : value <= 23.99 ? \"#9DC4DD80\" : value <= 25.58 ? \"#8DB0D580\" : value <= 27.17 ? \"#7D9DCD80\" : value <= 28.76 ? \"#6E89C480\" : value <= 30.35 ? \"#5E75BC80\" : value <= 31.94 ? \"#4E62B480\" : value <= 33.53 ? \"#3E4EAC80\" : value <= 35.12 ? \"#2F3AA380\" : value <= 36.71 ? \"#1F279B80\" : value <= 38.3 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 28.7 13.4 50.5 30.9
#> [2,] 32.9 11.4 35.3 36.3
#> [3,] 31.6  9.7 39.4 22.4
#> [4,] 34.7 10.2 43.1 38.3

```

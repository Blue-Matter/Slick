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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"12.7\" data-max=\"21.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"34\" data-max=\"90.9\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"21.9\" data-max=\"35.5\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"21.7\" data-max=\"66.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[12.7,14.5,21.6,16.2],[74,34,90.90000000000001,54.7],[21.9,35.5,23.2,28.4],[66.40000000000001,63.6,21.7,52.9]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 12.7 ? \"#ADD8E680\" : value <= 13.59 ? \"#9DC4DD80\" : value <= 14.48 ? \"#8DB0D580\" : value <= 15.37 ? \"#7D9DCD80\" : value <= 16.26 ? \"#6E89C480\" : value <= 17.15 ? \"#5E75BC80\" : value <= 18.04 ? \"#4E62B480\" : value <= 18.93 ? \"#3E4EAC80\" : value <= 19.82 ? \"#2F3AA380\" : value <= 20.71 ? \"#1F279B80\" : value <= 21.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 34 ? \"#ADD8E680\" : value <= 39.69 ? \"#9DC4DD80\" : value <= 45.38 ? \"#8DB0D580\" : value <= 51.07 ? \"#7D9DCD80\" : value <= 56.76 ? \"#6E89C480\" : value <= 62.45 ? \"#5E75BC80\" : value <= 68.14 ? \"#4E62B480\" : value <= 73.83 ? \"#3E4EAC80\" : value <= 79.52 ? \"#2F3AA380\" : value <= 85.21 ? \"#1F279B80\" : value <= 90.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 21.9 ? \"#ADD8E680\" : value <= 23.26 ? \"#9DC4DD80\" : value <= 24.62 ? \"#8DB0D580\" : value <= 25.98 ? \"#7D9DCD80\" : value <= 27.34 ? \"#6E89C480\" : value <= 28.7 ? \"#5E75BC80\" : value <= 30.06 ? \"#4E62B480\" : value <= 31.42 ? \"#3E4EAC80\" : value <= 32.78 ? \"#2F3AA380\" : value <= 34.14 ? \"#1F279B80\" : value <= 35.5 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 21.7 ? \"#ADD8E680\" : value <= 26.17 ? \"#9DC4DD80\" : value <= 30.64 ? \"#8DB0D580\" : value <= 35.11 ? \"#7D9DCD80\" : value <= 39.58 ? \"#6E89C480\" : value <= 44.05 ? \"#5E75BC80\" : value <= 48.52 ? \"#4E62B480\" : value <= 52.99 ? \"#3E4EAC80\" : value <= 57.46 ? \"#2F3AA380\" : value <= 61.93 ? \"#1F279B80\" : value <= 66.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"43.2\" data-max=\"61.9\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"37.3\" data-max=\"60.2\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"48.1\" data-max=\"58.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"20.4\" data-max=\"28\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[43.7,61.9,43.2,48.2],[57.7,51.3,60.2,37.3],[48.1,55.1,53,58.1],[28,23.9,20.4,21.3]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 43.2 ? \"#ADD8E680\" : value <= 45.07 ? \"#9DC4DD80\" : value <= 46.94 ? \"#8DB0D580\" : value <= 48.81 ? \"#7D9DCD80\" : value <= 50.68 ? \"#6E89C480\" : value <= 52.55 ? \"#5E75BC80\" : value <= 54.42 ? \"#4E62B480\" : value <= 56.29 ? \"#3E4EAC80\" : value <= 58.16 ? \"#2F3AA380\" : value <= 60.03 ? \"#1F279B80\" : value <= 61.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 37.3 ? \"#ADD8E680\" : value <= 39.59 ? \"#9DC4DD80\" : value <= 41.88 ? \"#8DB0D580\" : value <= 44.17 ? \"#7D9DCD80\" : value <= 46.46 ? \"#6E89C480\" : value <= 48.75 ? \"#5E75BC80\" : value <= 51.04 ? \"#4E62B480\" : value <= 53.33 ? \"#3E4EAC80\" : value <= 55.62 ? \"#2F3AA380\" : value <= 57.91 ? \"#1F279B80\" : value <= 60.2 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 48.1 ? \"#ADD8E680\" : value <= 49.1 ? \"#9DC4DD80\" : value <= 50.1 ? \"#8DB0D580\" : value <= 51.1 ? \"#7D9DCD80\" : value <= 52.1 ? \"#6E89C480\" : value <= 53.1 ? \"#5E75BC80\" : value <= 54.1 ? \"#4E62B480\" : value <= 55.1 ? \"#3E4EAC80\" : value <= 56.1 ? \"#2F3AA380\" : value <= 57.1 ? \"#1F279B80\" : value <= 58.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 20.4 ? \"#ADD8E680\" : value <= 21.16 ? \"#9DC4DD80\" : value <= 21.92 ? \"#8DB0D580\" : value <= 22.68 ? \"#7D9DCD80\" : value <= 23.44 ? \"#6E89C480\" : value <= 24.2 ? \"#5E75BC80\" : value <= 24.96 ? \"#4E62B480\" : value <= 25.72 ? \"#3E4EAC80\" : value <= 26.48 ? \"#2F3AA380\" : value <= 27.24 ? \"#1F279B80\" : value <= 28 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 43.7 57.7 48.1 28.0
#> [2,] 61.9 51.3 55.1 23.9
#> [3,] 43.2 60.2 53.0 20.4
#> [4,] 48.2 37.3 58.1 21.3

```

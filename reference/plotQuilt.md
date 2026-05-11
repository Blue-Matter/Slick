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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"29.3\" data-max=\"47.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"17.7\" data-max=\"54\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"17.2\" data-max=\"29.2\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"26.3\" data-max=\"70.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[29.6,29.3,47.6,31],[38.4,54,51.8,17.7],[17.2,19.7,29.2,22],[57.3,26.3,70.40000000000001,42.4]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 29.3 ? \"#ADD8E680\" : value <= 31.13 ? \"#9DC4DD80\" : value <= 32.96 ? \"#8DB0D580\" : value <= 34.79 ? \"#7D9DCD80\" : value <= 36.62 ? \"#6E89C480\" : value <= 38.45 ? \"#5E75BC80\" : value <= 40.28 ? \"#4E62B480\" : value <= 42.11 ? \"#3E4EAC80\" : value <= 43.94 ? \"#2F3AA380\" : value <= 45.77 ? \"#1F279B80\" : value <= 47.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 17.7 ? \"#ADD8E680\" : value <= 21.33 ? \"#9DC4DD80\" : value <= 24.96 ? \"#8DB0D580\" : value <= 28.59 ? \"#7D9DCD80\" : value <= 32.22 ? \"#6E89C480\" : value <= 35.85 ? \"#5E75BC80\" : value <= 39.48 ? \"#4E62B480\" : value <= 43.11 ? \"#3E4EAC80\" : value <= 46.74 ? \"#2F3AA380\" : value <= 50.37 ? \"#1F279B80\" : value <= 54 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 17.2 ? \"#ADD8E680\" : value <= 18.4 ? \"#9DC4DD80\" : value <= 19.6 ? \"#8DB0D580\" : value <= 20.8 ? \"#7D9DCD80\" : value <= 22 ? \"#6E89C480\" : value <= 23.2 ? \"#5E75BC80\" : value <= 24.4 ? \"#4E62B480\" : value <= 25.6 ? \"#3E4EAC80\" : value <= 26.8 ? \"#2F3AA380\" : value <= 28 ? \"#1F279B80\" : value <= 29.2 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 26.3 ? \"#ADD8E680\" : value <= 30.71 ? \"#9DC4DD80\" : value <= 35.12 ? \"#8DB0D580\" : value <= 39.53 ? \"#7D9DCD80\" : value <= 43.94 ? \"#6E89C480\" : value <= 48.35 ? \"#5E75BC80\" : value <= 52.76 ? \"#4E62B480\" : value <= 57.17 ? \"#3E4EAC80\" : value <= 61.58 ? \"#2F3AA380\" : value <= 65.99 ? \"#1F279B80\" : value <= 70.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"31.3\" data-max=\"48.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"50\" data-max=\"59.6\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"34.7\" data-max=\"43.9\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"20.8\" data-max=\"21.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[40.4,48.8,36.9,31.3],[56.7,59.6,51.9,50],[35.7,38.1,43.9,34.7],[21.3,21,20.8,21.8]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 31.3 ? \"#ADD8E680\" : value <= 33.05 ? \"#9DC4DD80\" : value <= 34.8 ? \"#8DB0D580\" : value <= 36.55 ? \"#7D9DCD80\" : value <= 38.3 ? \"#6E89C480\" : value <= 40.05 ? \"#5E75BC80\" : value <= 41.8 ? \"#4E62B480\" : value <= 43.55 ? \"#3E4EAC80\" : value <= 45.3 ? \"#2F3AA380\" : value <= 47.05 ? \"#1F279B80\" : value <= 48.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 50 ? \"#ADD8E680\" : value <= 50.96 ? \"#9DC4DD80\" : value <= 51.92 ? \"#8DB0D580\" : value <= 52.88 ? \"#7D9DCD80\" : value <= 53.84 ? \"#6E89C480\" : value <= 54.8 ? \"#5E75BC80\" : value <= 55.76 ? \"#4E62B480\" : value <= 56.72 ? \"#3E4EAC80\" : value <= 57.68 ? \"#2F3AA380\" : value <= 58.64 ? \"#1F279B80\" : value <= 59.6 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 34.7 ? \"#ADD8E680\" : value <= 35.62 ? \"#9DC4DD80\" : value <= 36.54 ? \"#8DB0D580\" : value <= 37.46 ? \"#7D9DCD80\" : value <= 38.38 ? \"#6E89C480\" : value <= 39.3 ? \"#5E75BC80\" : value <= 40.22 ? \"#4E62B480\" : value <= 41.14 ? \"#3E4EAC80\" : value <= 42.06 ? \"#2F3AA380\" : value <= 42.98 ? \"#1F279B80\" : value <= 43.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 20.8 ? \"#ADD8E680\" : value <= 20.9 ? \"#9DC4DD80\" : value <= 21 ? \"#8DB0D580\" : value <= 21.1 ? \"#7D9DCD80\" : value <= 21.2 ? \"#6E89C480\" : value <= 21.3 ? \"#5E75BC80\" : value <= 21.4 ? \"#4E62B480\" : value <= 21.5 ? \"#3E4EAC80\" : value <= 21.6 ? \"#2F3AA380\" : value <= 21.7 ? \"#1F279B80\" : value <= 21.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 40.4 56.7 35.7 21.3
#> [2,] 48.8 59.6 38.1 21.0
#> [3,] 36.9 51.9 43.9 20.8
#> [4,] 31.3 50.0 34.7 21.8

```

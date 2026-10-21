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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"27.5\" data-max=\"54.3\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"38.8\" data-max=\"55.2\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"39.1\" data-max=\"49.9\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"3.93\" data-max=\"8.28\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[27.5,54.3,33.9,39.5],[43.7,38.8,55.2,46.5],[49.9,45.2,39.1,48.4],[6.31,6.72,3.93,8.279999999999999]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 27.5 ? \"#ADD8E680\" : value <= 30.18 ? \"#9DC4DD80\" : value <= 32.86 ? \"#8DB0D580\" : value <= 35.54 ? \"#7D9DCD80\" : value <= 38.22 ? \"#6E89C480\" : value <= 40.9 ? \"#5E75BC80\" : value <= 43.58 ? \"#4E62B480\" : value <= 46.26 ? \"#3E4EAC80\" : value <= 48.94 ? \"#2F3AA380\" : value <= 51.62 ? \"#1F279B80\" : value <= 54.3 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 38.8 ? \"#ADD8E680\" : value <= 40.44 ? \"#9DC4DD80\" : value <= 42.08 ? \"#8DB0D580\" : value <= 43.72 ? \"#7D9DCD80\" : value <= 45.36 ? \"#6E89C480\" : value <= 47 ? \"#5E75BC80\" : value <= 48.64 ? \"#4E62B480\" : value <= 50.28 ? \"#3E4EAC80\" : value <= 51.92 ? \"#2F3AA380\" : value <= 53.56 ? \"#1F279B80\" : value <= 55.2 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 39.1 ? \"#ADD8E680\" : value <= 40.18 ? \"#9DC4DD80\" : value <= 41.26 ? \"#8DB0D580\" : value <= 42.34 ? \"#7D9DCD80\" : value <= 43.42 ? \"#6E89C480\" : value <= 44.5 ? \"#5E75BC80\" : value <= 45.58 ? \"#4E62B480\" : value <= 46.66 ? \"#3E4EAC80\" : value <= 47.74 ? \"#2F3AA380\" : value <= 48.82 ? \"#1F279B80\" : value <= 49.9 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 3.93 ? \"#ADD8E680\" : value <= 4.365 ? \"#9DC4DD80\" : value <= 4.8 ? \"#8DB0D580\" : value <= 5.235 ? \"#7D9DCD80\" : value <= 5.67 ? \"#6E89C480\" : value <= 6.105 ? \"#5E75BC80\" : value <= 6.54 ? \"#4E62B480\" : value <= 6.975 ? \"#3E4EAC80\" : value <= 7.41 ? \"#2F3AA380\" : value <= 7.845 ? \"#1F279B80\" : value <= 8.28 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}

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

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"29\" data-max=\"39.2\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"36.4\" data-max=\"54.8\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"14.2\" data-max=\"19.1\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"25.7\" data-max=\"34.4\" data-scale=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["MP 1","MP 2","MP 3","MP 4"],[29,37.4,39.2,33.2],[46.5,36.4,54.8,43],[18.8,19.1,15.5,14.2],[25.7,25.7,34.4,25.7]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PI1<\/th>\n      <th>PI2<\/th>\n      <th>PI3<\/th>\n      <th>PI4<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PI1","targets":1},{"name":"PI2","targets":2},{"name":"PI3","targets":3},{"name":"PI4","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 29 ? \"#ADD8E680\" : value <= 30.02 ? \"#9DC4DD80\" : value <= 31.04 ? \"#8DB0D580\" : value <= 32.06 ? \"#7D9DCD80\" : value <= 33.08 ? \"#6E89C480\" : value <= 34.1 ? \"#5E75BC80\" : value <= 35.12 ? \"#4E62B480\" : value <= 36.14 ? \"#3E4EAC80\" : value <= 37.16 ? \"#2F3AA380\" : value <= 38.18 ? \"#1F279B80\" : value <= 39.2 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 36.4 ? \"#ADD8E680\" : value <= 38.24 ? \"#9DC4DD80\" : value <= 40.08 ? \"#8DB0D580\" : value <= 41.92 ? \"#7D9DCD80\" : value <= 43.76 ? \"#6E89C480\" : value <= 45.6 ? \"#5E75BC80\" : value <= 47.44 ? \"#4E62B480\" : value <= 49.28 ? \"#3E4EAC80\" : value <= 51.12 ? \"#2F3AA380\" : value <= 52.96 ? \"#1F279B80\" : value <= 54.8 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 14.2 ? \"#ADD8E680\" : value <= 14.69 ? \"#9DC4DD80\" : value <= 15.18 ? \"#8DB0D580\" : value <= 15.67 ? \"#7D9DCD80\" : value <= 16.16 ? \"#6E89C480\" : value <= 16.65 ? \"#5E75BC80\" : value <= 17.14 ? \"#4E62B480\" : value <= 17.63 ? \"#3E4EAC80\" : value <= 18.12 ? \"#2F3AA380\" : value <= 18.61 ? \"#1F279B80\" : value <= 19.1 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 25.7 ? \"#ADD8E680\" : value <= 26.57 ? \"#9DC4DD80\" : value <= 27.44 ? \"#8DB0D580\" : value <= 28.31 ? \"#7D9DCD80\" : value <= 29.18 ? \"#6E89C480\" : value <= 30.05 ? \"#5E75BC80\" : value <= 30.92 ? \"#4E62B480\" : value <= 31.79 ? \"#3E4EAC80\" : value <= 32.66 ? \"#2F3AA380\" : value <= 33.53 ? \"#1F279B80\" : value <= 34.4 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}apply(quilt@Value, 3:4, mean) |> round(1)
#>      [,1] [,2] [,3] [,4]
#> [1,] 29.0 46.5 18.8 25.7
#> [2,] 37.4 36.4 19.1 25.7
#> [3,] 39.2 54.8 15.5 34.4
#> [4,] 33.2 43.0 14.2 25.7



# Example `Slick` object included with the package (see `?WSKJ`)
plotQuilt(WSKJ)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.695\" data-max=\"0.714\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.732\" data-max=\"0.759\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.736\" data-max=\"0.763\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"30000\" data-max=\"31900\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["CE","IR","SP","SPAH"],[0.708,0.714,0.707,0.695],[0.759,0.756,0.748,0.732],[0.759,0.763,0.752,0.736],[30700,30900,30000,31900]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PGK<\/th>\n      <th>P100<\/th>\n      <th>PNOF<\/th>\n      <th>Mean TAC<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PGK","targets":1},{"name":"P100","targets":2},{"name":"PNOF","targets":3},{"name":"Mean TAC","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.695 ? \"#ADD8E680\" : value <= 0.6969 ? \"#9DC4DD80\" : value <= 0.6988 ? \"#8DB0D580\" : value <= 0.7007 ? \"#7D9DCD80\" : value <= 0.7026 ? \"#6E89C480\" : value <= 0.7045 ? \"#5E75BC80\" : value <= 0.7064 ? \"#4E62B480\" : value <= 0.7083 ? \"#3E4EAC80\" : value <= 0.7102 ? \"#2F3AA380\" : value <= 0.7121 ? \"#1F279B80\" : value <= 0.714 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.732 ? \"#ADD8E680\" : value <= 0.7347 ? \"#9DC4DD80\" : value <= 0.7374 ? \"#8DB0D580\" : value <= 0.7401 ? \"#7D9DCD80\" : value <= 0.7428 ? \"#6E89C480\" : value <= 0.7455 ? \"#5E75BC80\" : value <= 0.7482 ? \"#4E62B480\" : value <= 0.7509 ? \"#3E4EAC80\" : value <= 0.7536 ? \"#2F3AA380\" : value <= 0.7563 ? \"#1F279B80\" : value <= 0.759 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.736 ? \"#ADD8E680\" : value <= 0.7387 ? \"#9DC4DD80\" : value <= 0.7414 ? \"#8DB0D580\" : value <= 0.7441 ? \"#7D9DCD80\" : value <= 0.7468 ? \"#6E89C480\" : value <= 0.7495 ? \"#5E75BC80\" : value <= 0.7522 ? \"#4E62B480\" : value <= 0.7549 ? \"#3E4EAC80\" : value <= 0.7576 ? \"#2F3AA380\" : value <= 0.7603 ? \"#1F279B80\" : value <= 0.763 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 30000 ? \"#ADD8E680\" : value <= 30190 ? \"#9DC4DD80\" : value <= 30380 ? \"#8DB0D580\" : value <= 30570 ? \"#7D9DCD80\" : value <= 30760 ? \"#6E89C480\" : value <= 30950 ? \"#5E75BC80\" : value <= 31140 ? \"#4E62B480\" : value <= 31330 ? \"#3E4EAC80\" : value <= 31520 ? \"#2F3AA380\" : value <= 31710 ? \"#1F279B80\" : value <= 31900 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}
```

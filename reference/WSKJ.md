# Western Atlantic Skipjack Tuna MSE Results

A `Slick` data object containing MSE results for the western Atlantic
skipjack tuna (*Katsuwonus pelamis*) used as a case study in Hordyk et
al. (in review). The object is derived from the WSKJ case study
available in the Slick App, with Boxplot, Quilt, Spider, and Tradeoff
slots populated from the Timeseries data to demonstrate all six plot
types supported by the package. Performance indicator values for the
additional plot types are summarized as means over the projection period
(years 2026–2055).

## Usage

``` r
WSKJ
```

## Format

An object of class
[Slick](https://slick.bluematterscience.com/reference/Slick-class.md)
with all six plot slots populated: Timeseries, Kobe, Boxplot, Quilt,
Spider, and Tradeoff.

## Source

Downloaded via
[`download_casestudy()`](https://slick.bluematterscience.com/reference/get_casestudies.md)
and extended for illustrative purposes.

## See also

[`download_casestudy()`](https://slick.bluematterscience.com/reference/get_casestudies.md),
[`App()`](https://slick.bluematterscience.com/reference/App.md)

## Examples

``` r
# Summary of the object
WSKJ
#> 
#> ── An object of class `Slick` ──────────────────────────────────────────────────
#> 
#> ── `Title` ──
#> 
#> ℹ Multi-language List
#> 
#> ── en 
#> 1. Western Atlantic Skipjack Tuna (Katsuwon ...
#> 
#> ── es 
#> 1. Evaluación de Estrategias de Manejo del ...
#> 
#> ── fr 
#> 1. Évaluation des Stratégies de Gestion du ...
#> 
#> ── `Subtitle` ──
#> 
#> ℹ Multi-language List
#> 
#> ── en 
#> 1. Final results presented during the SCRS ...
#> 
#> ── es 
#> 1. Resultados finales presentados durante l ...
#> 
#> ── fr 
#> 1. Résultats finaux présentés lors de la Ré ...
#> 
#> ── `Date` ──
#> 
#> September 2025
#> 
#> ── `Author` ──
#> 
#> 1 Rodrigo Sant’Ana
#> 2 Bruno Mourato
#> 3 Adrian Hordyk
#> 
#> ── `Email` ──
#> 
#> 1 rsantana@univali.br
#> 
#> ── `Institution` ──
#> 
#> 1 Universidade do Vale do Itajaí
#> 2 Universidade Federal de São Paulo
#> 3 Blue Matter Science
#> 
#> ── `Introduction` ──
#> 
#> ℹ Multi-language List
#> 
#> ── en 
#> 1. The SCRS’s Tropical Tunas Species Group ...
#> 
#> ── es 
#> 1. El Grupo de Especies de Atunes Tropicale ...
#> 
#> ── fr 
#> 1. Le Groupe d’Espèces des Thons Tropicaux ...
#> 
#> ── `MPs` ──
#> 
#> ✔ Complete
#> 
#> ── `OMs` ──
#> 
#> ✔ Complete
#> 
#> ── `Boxplot` ──
#> 
#> ✔ Complete
#> 
#> ── `Quilt` ──
#> 
#> ✔ Complete
#> 
#> ── `Kobe` ──
#> 
#> ✔ Complete
#> 
#> ── `Spider` ──
#> 
#> ✔ Complete
#> 
#> ── `Timeseries` ──
#> 
#> ✔ Complete
#> 
#> ── `Tradeoff` ──
#> 
#> ✔ Complete

# Check the object is valid
Check(WSKJ)
#> 
#> ── Checking: "Slick" ──
#> 
#> ── Checking: "MPs" 
#> ✔ Complete
#> 
#> ── Checking: "OMs" 
#> ✔ Complete
#> 
#> ── Checking: "Boxplot" 
#> ✔ Complete
#> 
#> ── Checking: "Quilt" 
#> ✔ Complete
#> 
#> ── Checking: "Kobe" 
#> ✔ Complete
#> 
#> ── Checking: "Spider" 
#> ✔ Complete
#> 
#> ── Checking: "Timeseries" 
#> ✔ Complete
#> 
#> ── Checking: "Tradeoff" 
#> ✔ Complete

# Metadata, Management Procedures, and Operating Models
Title(WSKJ)
#> [1] "Western Atlantic Skipjack Tuna (Katsuwonus pelamis) Management Strategy Evaluation"
Metadata(MPs(WSKJ))
#>   Code                                  Label
#> 1   CE                  Constant Exploitation
#> 2   IR                            Index Ratio
#> 3   SP     Surplus Production with linear HCR
#> 4 SPAH Surplus Production with non-linear HCR
#>                                                                                                                                                                               Description
#> 1 The TAC is adjusted based on the ratio of the estimated current exploitation rate to the mean exploitation rate from 2016 - 2017, with the aim to maintain a constant exploitation rate
#> 2                                                   The TAC is adjusted based on the ratio of the mean index over the 3 most recent years to the value of the mean index from 2019 - 2021
#> 3                                                                 A model-based a surplus production model with a 130-60 hockey stick harvest control rule and an F_Target equal to F_MSY
#> 4                                                                                                     The same surplus production model as SP, but with a non-linear harvest control rule
#>     Color
#> 1 #C87A8A
#> 2 #909646
#> 3 #00A396
#> 4 #9189C7
Factors(OMs(WSKJ))
#>      Factor     Level Description
#> 1    Growth     Qnt25            
#> 2    Growth     Qnt50            
#> 3    Growth     Qnt75            
#> 4 Steepness        h6            
#> 5 Steepness        h7            
#> 6 Steepness        h8            
#> 7      Type Reference            
Design(OMs(WSKJ))
#>   Growth Steepness      Type
#> 1  Qnt25        h6 Reference
#> 2  Qnt25        h7 Reference
#> 3  Qnt25        h8 Reference
#> 4  Qnt50        h6 Reference
#> 5  Qnt50        h7 Reference
#> 6  Qnt50        h8 Reference
#> 7  Qnt75        h6 Reference
#> 8  Qnt75        h7 Reference
#> 9  Qnt75        h8 Reference

# Plots
plotTimeseries(WSKJ)

plotBoxplot(WSKJ)

plotKobe(WSKJ)

plotQuilt(WSKJ)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td><\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.695\" data-max=\"0.714\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.732\" data-max=\"0.759\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0.736\" data-max=\"0.763\" data-scale=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"30000\" data-max=\"31900\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","extensions":["Buttons"],"data":[["CE","IR","SP","SPAH"],[0.708,0.714,0.707,0.695],[0.759,0.756,0.748,0.732],[0.759,0.763,0.752,0.736],[30700,30900,30000,31900]],"container":"<table class=\"FALSE\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>PGK<\/th>\n      <th>P100<\/th>\n      <th>PNOF<\/th>\n      <th>Mean TAC<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"tB","pageLength":100,"buttons":["copy","csv"],"columnDefs":[{"className":"dt-center","targets":"_all"},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"PGK","targets":1},{"name":"P100","targets":2},{"name":"PNOF","targets":3},{"name":"Mean TAC","targets":4}],"scrollX":true,"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true,"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.695 ? \"#ADD8E680\" : value <= 0.6969 ? \"#9DC4DD80\" : value <= 0.6988 ? \"#8DB0D580\" : value <= 0.7007 ? \"#7D9DCD80\" : value <= 0.7026 ? \"#6E89C480\" : value <= 0.7045 ? \"#5E75BC80\" : value <= 0.7064 ? \"#4E62B480\" : value <= 0.7083 ? \"#3E4EAC80\" : value <= 0.7102 ? \"#2F3AA380\" : value <= 0.7121 ? \"#1F279B80\" : value <= 0.714 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[2]; $(this.api().cell(row, 2).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.732 ? \"#ADD8E680\" : value <= 0.7347 ? \"#9DC4DD80\" : value <= 0.7374 ? \"#8DB0D580\" : value <= 0.7401 ? \"#7D9DCD80\" : value <= 0.7428 ? \"#6E89C480\" : value <= 0.7455 ? \"#5E75BC80\" : value <= 0.7482 ? \"#4E62B480\" : value <= 0.7509 ? \"#3E4EAC80\" : value <= 0.7536 ? \"#2F3AA380\" : value <= 0.7563 ? \"#1F279B80\" : value <= 0.759 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[3]; $(this.api().cell(row, 3).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 0.736 ? \"#ADD8E680\" : value <= 0.7387 ? \"#9DC4DD80\" : value <= 0.7414 ? \"#8DB0D580\" : value <= 0.7441 ? \"#7D9DCD80\" : value <= 0.7468 ? \"#6E89C480\" : value <= 0.7495 ? \"#5E75BC80\" : value <= 0.7522 ? \"#4E62B480\" : value <= 0.7549 ? \"#3E4EAC80\" : value <= 0.7576 ? \"#2F3AA380\" : value <= 0.7603 ? \"#1F279B80\" : value <= 0.763 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[4]; $(this.api().cell(row, 4).node()).css({'background-color':isNaN(parseFloat(value)) ? '' : value <= 30000 ? \"#ADD8E680\" : value <= 30190 ? \"#9DC4DD80\" : value <= 30380 ? \"#8DB0D580\" : value <= 30570 ? \"#7D9DCD80\" : value <= 30760 ? \"#6E89C480\" : value <= 30950 ? \"#5E75BC80\" : value <= 31140 ? \"#4E62B480\" : value <= 31330 ? \"#3E4EAC80\" : value <= 31520 ? \"#2F3AA380\" : value <= 31710 ? \"#1F279B80\" : value <= 31900 ? \"#0F139380\" : \"#00008B80\"});\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold'});\n}"},"selection":{"mode":"none","selected":null,"target":"row","selectable":null}},"evals":["options.rowCallback"],"jsHooks":[]}plotSpider(WSKJ)

plotTradeoff(WSKJ)


if (FALSE) { # interactive()
# Explore the results in the Slick App
App(slick=WSKJ)
}
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# Slick

<!-- badges: start -->

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/Slick)](https://CRAN.R-project.org/package=Slick) -->

[![R-CMD-check](https://github.com/Blue-Matter/Slick/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Blue-Matter/Slick/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`Slick` is an R package designed for visualizing and exploring results
of a Management Strategy Evaluation (MSE).

MSE involves using closed-loop simulation testing to evaluate and
compare the expected performance of alternative management methods,
often call **Management Procedures (MPs)**, across different hypotheses
of the historical and future fishery dynamics, referred to as
**Operating Models (OMs)**. The results of an MSE are reported in a set
of **Performance Indicators (PIs)**, quantitative measures of the
management objectives of fishery stakeholders, managers, and other
decision-makers.

`Slick` allows MSE developers and analysts to prepare their MSE results
in a standardized data structure, easily generate publication quality
figures and tables, and present and explore the MSE results in an
interactive application.

## Installation

`Slick` can be installed from CRAN:

``` r
install.packages('Slick')
```

The development version can be installed from Github:

``` r
# install.packages('pak')
pak::pkg_install('blue-matter/Slick')
library(Slick)
```

## The Slick object

To use `Slick`, the results of an MSE must be available in a Slick data
object. The Slick App includes several example data objects. See the
[Developer’s
Guide](https://slick.bluematterscience.com/articles/DevelopersGuide.html)
for documentation on creating a Slick Data object for your MSE results.

## The Slick App

The Slick App has been designed for interactively examining the MSE
results contained within a Slick data object.

The App allows for the simultaneous presentation of various performance
metrics and can account for uncertainty in the states of nature. Slick
is interactive and allows users to filter results live in order to
explore robustness and performance.

While Slick can be applied to any decision analysis context it was
specifically designed to investigate the performance of fisheries
management procedures tested by management strategy evaluation (MSE).

Importantly the App is platform agnostic: results arising from any MSE
framework that are formatted in a compatible Slick object can be loaded
and visualized in the App.

### Running the App locally

The Slick App can be run locally or accessed online.

To run the App locally, simply run `App()`.

The example data objects can be loaded within the App, or a Slick data
object can be uploaded.

If you have your own Slick data object, this object can be loaded
directly with:

``` r
App(slick=mySlick)
```

where `mySlick` is a completed Slick data object.

### Online App

The online version of the Slick App can be accessed
[here](https://shiny.bluematterscience.com/app/slick).

### About Slick

Slick was developed by [Blue Matter
Science](https://www.bluematterscience.com) and designed and
commissioned by [The Ocean Foundation’s](https://oceanfdn.org/)
International Fisheries Conservation Project and
[harveststrategies.org](https://harveststrategies.org/), with support
from [The Pew Charitable Trusts](https://www.pew.org/en/), and the
[Common Oceans Tuna Fisheries
Project](https://www.fao.org/in-action/commonoceans/what-we-do/tuna-fisheries/en/),
which is funded by
[GEF](https://www.thegef.org/what-we-do/topics/areas-beyond-national-jurisdiction)
and implemented by the
[FAO](https://www.fao.org/in-action/commonoceans/en/).

The prototype figure designs were developed by [5W
Infographics](https://www.5wgraphics.com/).

`Slick` is under going further development. All feedback is welcome.

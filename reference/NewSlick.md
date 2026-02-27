# Creates a blank Slick object

Creates a blank Slick object

## Usage

``` r
NewSlick(
  name = "Unnamed Slick Object",
  nPerf = list(nD = 5, nS = 6, nP = 7),
  nMPs = 5,
  nsim = 10,
  nProjYr = 50,
  nStateVar = 2,
  nHistYr = 55,
  Design = expand.grid(1:2, 1:2)
)
```

## Arguments

- name:

  Character string that is the object name (shortened for use in menus
  etc.)

- nPerf:

  An integer vector of the number of deterministic (nD), stochastic (nS)
  and projected (nP) performance metrics

- nMPs:

  Integer, the number of management options (aka management procedures).

- nsim:

  Integer, the number of simulations (stochastic replicates per state of
  nature)

- nProjYr:

  Integer, the number of projected years

- nStateVar:

  Integer, the number of state variables

- nHistYr:

  Integer, the number of historical years for state variables

- Design:

  A design matrix of factor levels `SN, factor`

## Value

An object of class
[Slick](https://slick.bluematterscience.com/reference/Slick-class.md)

## Author

T. Carruthers

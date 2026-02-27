# Make an example Slick object

A function that creates an example Slick data object

## Usage

``` r
Make_Slick(
  name = "Unnamed Slick object",
  OM = NULL,
  MPs = c("DCAC", "AvC", "Fratio", "FMSYref", "FMSYref50", "matlenlim"),
  MP_Desc = NULL,
  PMs = c("AAVE", "AAVY", "LTY", "P10", "P50", "P100", "PNOF", "STY", "Yield"),
  Design = as.data.frame(cbind(rbind(as.matrix(expand.grid(1:2, 1:3, 1:2)), matrix(1,
    nrow = 5, ncol = 3)), c(rep(1, 12), 2:6))),
  SN = list(Factor_Labels = c("Natural Mortality", "Resilience", "Stock Depletion",
    "Robustness"), Labels = list(c("M=0.2", "M=0.3"), c("h=0.5", "h=0.7", "h=0.9"),
    c("Dep=0.1", "Dep=0.3"), c("Ref_Case", "L50=0.5", "Vmaxlen=0.1", "Cobs=0.5",
    "Perr=0.5", "AC=0.95")), Codes = list(c("M2", "M3"), c("h5", "h7", "h9"), c("D1",
    "D3"), c("Ref_case", "mat_low", "dome", "h_Cerr", "h_Perr", "h_AC")), Description =
    list(c("M=0.2", "M=0.3"), c("h=0.5", "h=0.7", "h=0.9"), c("Dep=0.1", "Dep=0.3"),
    c("Reference Case", 
     "L50=0.5", "Vmaxlen=0.1", "Cobs=0.5", "Perr=0.5",
    "AC=0.95"))),
  mods = list(function(OM, lev) {
if (lev == 1) OM@M <- c(0.2, 0.2)
     if (lev ==
    2) OM@M <- c(0.3, 0.3)
     OM
 }, function(OM, lev) {
     if (lev == 1) OM@h <-
    c(0.5, 0.5)
if (lev == 2) OM@h = c(0.7, 0.7)
     if (lev == 3) OM@h = c(0.9,
    0.9)
     OM
 }, function(OM, lev) {
if (lev == 1) OM@D <- c(0.1, 0.1)
     if
    (lev == 2) OM@D <- c(0.3, 0.3)
     OM
 }, function(OM, lev) {
     if (lev == 2)
OM@L50 <- c(0.5, 0.5)
if (lev == 3) OM@Vmaxlen = c(0.1, 0.1)
     if (lev == 4)
    OM@Cobs = c(0.5, 0.5)
if (lev == 5) OM@Perr = c(0.5, 0.5)
     if (lev == 6)
    OM@AC = c(0.9, 0.9)
     OM
 }),
  nsim = 48,
  MSElist = NULL,
  fstYr = NULL,
  returnMSEs = FALSE
)
```

## Arguments

- name:

  Character string that is the object name (abbreviated for use in menus
  etc)

- OM:

  An operating model object (class 'OM')

- MPs:

  A vector of methods (character string) of class MP

- MP_Desc:

  A vector method descriptions (character string) nMPs long

- PMs:

  A vector of performance metrics of class PM

- Design:

  A design matrix of OM runs

- SN:

  A list of Labels, Codes and Descriptions of the factor levels. Each
  list item is a factor containing a vector of factor levels.

- mods:

  A nested list of mods

- nsim:

  Integer, the number of simulations

- MSElist:

  An optional list of prerun MSEs

- fstYr:

  An optional numeric value for first projection year. Otherwise current
  year is used

- returnMSEs:

  Logical, rather than the Slick object should the list of MSEs be
  returned?

## Value

An object of class
[Slick](https://slick.bluematterscience.com/reference/Slick-class.md)

## Author

T. Carruthers

This is a re-submission of a new release in response to the CRAN Team's 
review of the initial submission.

Thank you for taking the time to review our package.

All issues identified by CRAN Team have been fixed. 

The following changes have been made:

- DESCRIPTION: added single quotes around `Slick` in Description field
- DESCRIPTION: added <https://slick.bluematterscience.com/ in Description field 
  to reference the `Slick` website.
  A manuscript describing this package will be submitted to a scientific journal 
  in the next few months. Once it has been published, we will add the
  <doi:..> to the Description field for the next CRAN release.
- Replaced all instances of "T" and "F" in the code with "TRUE" and "FALSE"
- Added \value tag with description of returned object to: Code.Rd, Color.Rd, 
  Defaults.Rd, Preset.Rd, RefPoints.Rd, show.Rd, Target.Rd, Time.Rd, TimeLab.Rd,
  TimeTerminal.Rd, Value.Rd
- Replaced one instance of `print(...)` with `message(...)` in 'R/make_Slick_old.R'. 
  Deleted `print(...)` in R/mod_Spider_MP.R. There are no other calls to `print` 
  or `cat` outside of `show` method
- Replaced `options(shiny.maxRequestSize=200*1024^2)` in R/app_server.R with
  `options(shiny.maxRequestSize = getOption("shiny.maxRequestSize", 200*1024^2))`
- Deleted unnecessary `options(shiny.maxRequestSize=200*1024^2)` in R/mod_Home.R 
- Edited code in R/RunSlick.R to make clear user's options are not being changed. 
- Added `oldpar <- par(no.readonly = TRUE)` and `on.exit(par(oldpar))` to
  R/mod_Spider.R and R/fct_plotSpider.R

## Test environments

* local - Windows 4.5.1
* Github actions - macOS-latest (release)
* Github actions - windows-latest (release)
* Github actions - ubuntu-latest (devel)
* Github actions - ubuntu-latest (release)
* Github actions - ubuntu-latest (oldrel-1)

## R CMD check results

### Local Test

0 errors | 1 warnings | 1 notes

WARNING: LaTeX errors when creating PDF version. 
Due to this issue: https://github.com/rstudio/rstudio/issues/15805

NOTE: New submission

### Remote Test Environments

0 errors | 0 warnings | 0 notes




 




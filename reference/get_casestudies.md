# List and Download the Example Case Studies

The example case studies shown in the
[`App()`](https://slick.bluematterscience.com/reference/App.md) are
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
objects stored in the SlickLibrary GitHub repository:
<https://github.com/Blue-Matter/SlickLibrary>. `get_casestudies()` lists
the available case studies, and `download_casestudy()` downloads a case
study and loads it into the R session.

## Usage

``` r
get_casestudies()

download_casestudy(
  name,
  case_studies = NULL,
  dir = NULL,
  silent = FALSE,
  object = TRUE,
  delete = object
)
```

## Arguments

- name:

  Character. The name of the case study to download, from the `Name`
  column returned by `get_casestudies()`

- case_studies:

  Optional. The `data.frame` returned by `get_casestudies()`. If not
  provided, `get_casestudies()` is called.

- dir:

  Optional. Directory to save the downloaded file. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)

- silent:

  Logical. Hide the download messages?

- object:

  Logical. Load and return the `Slick` object?

- delete:

  Logical. Delete the downloaded file when the function finishes?
  Defaults to the value of `object`. Use `object=FALSE, delete=FALSE` to
  only save the file.

## Value

`get_casestudies()` returns a `data.frame` with one row per case study
and columns:

- Name:

  The name of the case study. Use this for the `name` argument of
  `download_casestudy()`

- File:

  The path of the file in the SlickLibrary repository

- Size:

  The size of the file in MB

`download_casestudy()` returns the
[`Slick-class()`](https://slick.bluematterscience.com/reference/Slick-class.md)
object if `object=TRUE`, otherwise `NULL` (invisibly), and the file is
saved in `dir`.

## Details

Both functions require an internet connection and the `httr` package.

## Functions

- `download_casestudy()`: Download a case study and return the `Slick`
  object

## See also

[WSKJ](https://slick.bluematterscience.com/reference/WSKJ.md) for an
example `Slick` object included with the package

## Examples

``` r
if (FALSE) { # interactive()
# List the available case studies
case_studies <- get_casestudies()
case_studies

# Download a case study and load it into the R session
slick <- download_casestudy(case_studies$Name[1], case_studies)
slick

# Save the case study file to the working directory instead
download_casestudy(case_studies$Name[1], case_studies, dir=getwd(),
                   object=FALSE, delete=FALSE)
}
```

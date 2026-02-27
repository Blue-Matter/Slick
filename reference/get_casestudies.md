# Get Case Studies from the SlickLibrary GitHub Repo

Get Case Studies from the SlickLibrary GitHub Repo

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

  The name of the case study to download. `Name` column from
  `get_casestudies()`

- case_studies:

  optional. Dataframe returned by `get_casestudies()`

- dir:

  Optional. Directory to save the file. Defaults to a temporary
  directory

- silent:

  Logical. Print out messages?

- object:

  Logical. Return the Slick object? Default downloads Slick object to a
  temporary location, loads and returns the Slick object, and then
  deletes downloaded file.

- delete:

  Logical. Delete the downloaded file after function finishes? Only
  useful if `object = TRUE`

## Value

A data.frame for `get_casestudies` and a `Slick` object for
`download_casestudy`

The downloaded `Slick` object if `object==TRUE`, otherwise nothing.

## Functions

- `download_casestudy()`: download a case study file

## Examples

``` r
if (FALSE) { # interactive()
case_studies <- get_casestudies()
slick <- download_casestudy(case_studies$Name[1])
}
```

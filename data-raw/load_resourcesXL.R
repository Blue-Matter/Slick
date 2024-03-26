
Resources <- readxl::read_xlsx('data-raw/Resources.xlsx')
usethis::use_data(Resources, overwrite = TRUE)

Footnotes <- readxl::read_xlsx('data-raw/Resources.xlsx', sheet="Footnotes")
usethis::use_data(Footnotes, overwrite = TRUE)

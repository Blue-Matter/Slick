

Resources <- readxl::read_xlsx('data-raw/Resources.xlsx')
Footnotes <- readxl::read_xlsx('data-raw/Resources.xlsx', sheet="Footnotes")



case_study_files <- list.files('data-raw/case_studies')
case_study_objects <- tools::file_path_sans_ext(case_study_files)
case_study_df <- data.frame(Example=c('North Atlantic Swordfish',
                                      'Demonstration',
                                      'Western Atlantic Skipjack Tuna'),
                            Object=case_study_objects,
                            Order=c(2,1,3))

case_study_df <- case_study_df |> dplyr::arrange(Order)

language_codes <- read.csv('data-raw/language-codes.csv')

# sysdata.rda
usethis::use_data(language_codes, case_study_df, Resources, Footnotes, overwrite = TRUE, internal = TRUE)


# save case study files to inst directory
for (i in seq_along(case_study_objects)) {
  obj <- readRDS(file.path('data-raw/case_studies', case_study_files[i]))
  name <- case_study_objects[i]
  saveRDS(obj, paste0('inst/', name, '.rda'))
}

library(usethis)
case_study_files <- list.files('data-raw/case_studies')

case_study_objects <- tools::file_path_sans_ext(case_study_files)

for (i in seq_along(case_study_objects)) {
  obj <- readRDS(file.path('data-raw/case_studies', case_study_files[i]))
  name <- case_study_objects[i]
  assign(name, obj)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
}




case_study_df <- data.frame(Example=c('North Atlantic Swordfish',
                                      'Demonstration',
                                      'Western Atlantic Skipjack Tuna'),
                            Object=case_study_objects,
                            Order=c(2,1,3))

case_study_df <- case_study_df |> dplyr::arrange(Order)

usethis::use_data(case_study_df, overwrite = TRUE)

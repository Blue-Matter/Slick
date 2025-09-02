
MissingTags <- checkhelper::find_missing_tags()

MissingTags$package_doc

MissingTags$data

MissingTags$functions |> dplyr::filter(has_export==TRUE, has_return==FALSE)


devtools::check(remote = TRUE, manual = TRUE)

devtools::check_win_devel()

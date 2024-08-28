
# ---- escape unicode in translation files ----
# library(stringi)
# files <- list.files('inst/translations', full.names = TRUE)
#
# for (fl in files) {
#   csv <- read.csv(fl, fileEncoding =  'latin1')
#   trans <- csv[,2]
#   escape_trans <- rep(NA, length(trans))
#   for (i in seq_along(trans)) {
#     escape_trans[i] <- stringi::stri_escape_unicode(trans[i])
#   }
#   csv[,2] <- trans
#   write.csv(csv, fl, row.names = FALSE)
#
#
# }
#

# create_translation_file(path="inst/shiny_apps/Slick/Source/Pages/HomeDEV.r",
#                         type = "json", handle = "i18n",
#                         output = NULL)

prepare_translation <- function() {
  files <- list.files('R', recursive = TRUE, full.names = TRUE)
  files <- files[!grepl('sysdata', files)]

  # create temp translation files from all scripts
  for (fl in files) {
    nm <- basename(fl) |> tools::file_path_sans_ext() |> paste0('.csv')
    out_file <- file.path('build/translation_temp', nm)
    shiny.i18n::create_translation_file(path=fl,
                                        type='csv',
                                        output=out_file)
  }

  # create master translation file
  if(!file.exists('build/translation_en_master.csv')) {
    file.create(file.path('build/translation_en_master.csv'))
  }

  files <- list.files('build/translation_temp', recursive = TRUE, full.names = TRUE)

  importCSV <- function(fl) {
    data <- read.csv(fl)
    if (nrow(data)>0) return(data)
  }

  df <- do.call('rbind', lapply(files, importCSV))
  rownames(df) <- NULL

  colnames(df)[1] <- 'en'
  df <- df |> dplyr::distinct(en)

  write.csv(df, 'build/translation_en_master.csv', row.names = FALSE)

}


prepare_translation()

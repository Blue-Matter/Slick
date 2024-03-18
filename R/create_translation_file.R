

# create_translation_file(path="inst/shiny_apps/Slick/Source/Pages/HomeDEV.r",
#                         type = "json", handle = "i18n",
#                         output = NULL)

prepare_translation <- function() {
  files <- list.files('inst/shiny_apps/Slick/Source', recursive = TRUE, full.names = TRUE)

  # create temp translation files from all scripts
  for (fl in files) {
    nm <- basename(fl) %>% tools::file_path_sans_ext() %>% paste0(., '.csv')
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

  df <- sapply(files, importCSV) %>% do.call('rbind',.)
  rownames(df) <- NULL
  df

  df <- df %>% distinct(key) %>% arrange(key)
  colnames(df)[1] <- 'en'

  write.csv(df, 'build/translation_en_master.csv', row.names = FALSE)

  # ---- escape unicode in translation files ----
  library(stringi)
  files <- list.files('inst/shiny_apps/Slick/data/translations', full.names = TRUE)

  for (fl in files) {
    csv <- read.csv(fl, fileEncoding =  'latin1')
    trans <- csv[,2]
    escape_trans <- rep(NA, length(trans))
    for (i in seq_along(trans)) {
      escape_trans[i] <- stringi::stri_escape_unicode(trans[i])
    }
    csv[,2] <- trans
    write.csv(csv, fl, row.names = FALSE)


  }




}

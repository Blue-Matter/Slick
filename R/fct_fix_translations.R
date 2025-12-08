

FixTranslations <- function() {
  dir <- 'inst/translations'
  fls <- list.files(dir)

  fls <- fls[!fls=="translation_es.csv"]

  es <- read.csv(file.path(dir,"translation_es.csv"))

  es_start_1 <- which(startsWith(es[,1],' '))
  es_start_2 <- which(startsWith(es[,2],' '))
  es_end_1 <- which(endsWith(es[,1],' '))
  es_end_2 <- which(endsWith(es[,2],' '))


  others <- lapply(file.path(dir, fls), read.csv)
  nms <- gsub("translation_", '', fls)
  nms <- gsub(".csv", '', nms)

  names(others) <- nms

  for (i in seq_along(others)) {
    lang <- others[[i]]
    lang[,1] <- gsub("\U00A0", " ", lang[,1])
    lang[,2] <- gsub("\U00A0", " ", lang[,2])

    if (nrow(es) != nrow(lang)) {
      cli::cli_abort(' {.val {nms[i]}} does not have same number of entries as {.val {"es"}}')
    }

    start_1 <- which(startsWith(lang[,1],' '))
    chk <- all(es_start_1 %in% start_1)
    if (!chk) {
      rr <- es_start_1[which(!es_start_1 %in% start_1)]
      lang[rr,1] <- paste0(' ',  lang[rr,1])
    }

    start_2 <- which(startsWith(lang[,2],' '))
    chk <- all(es_start_2 %in% start_2)
    if (!chk) {
      rr <- es_start_2[which(!es_start_2 %in% start_2)]
      lang[rr,2] <- paste0(' ',  lang[rr,2])
    }

    end_1 <- which(endsWith(lang[,1],' '))
    chk <- all(es_end_1 %in% end_1)
    if (!chk) {
      rr <- es_end_1[which(!es_end_1 %in% end_1)]
      lang[rr,1] <- paste0(lang[rr,1], ' ')
    }

    end_2 <- which(endsWith(lang[,2],' '))
    chk <- all(es_end_2 %in% end_2)
    if (!chk) {
      rr <- es_end_2[which(!es_end_2 %in% end_2)]
      lang[rr,2] <- paste0(lang[rr,2], ' ')
    }

    write.csv(lang, file.path(dir, fls[i]), row.names = FALSE)
  }



}

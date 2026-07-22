
format_check_errors <- function(errors) {
  lines <- character(0)
  nms <- names(errors)
  for (i in seq_along(errors)) {
    sub <- unlist(errors[[i]])
    if (length(sub)<1) next
    nm <- nms[i]
    if (!is.null(nm) && nchar(nm)>0) {
      lines <- c(lines, paste0(nm, ': ', sub))
    } else {
      lines <- c(lines, sub)
    }
  }
  paste(lines, collapse='\n')
}


invalid_slick_alert <- function(err=NULL, detail=NULL) {
  if (!is.null(err)) detail <- conditionMessage(err)
  text <- 'Use `Check(slick_object)` to see the errors'
  if (!is.null(detail) && nchar(detail)>0)
    text <- paste0(text, '. Details: ', detail)
  shinyalert::shinyalert('Invalid Slick object', text, type='error')
  NULL
}

nPI_count <- function(object) {
  if (is.list(object@Code)) {
    if (length(object@Code[[1]])>0 && all(nchar(object@Code[[1]])>0))
      return(length(object@Code[[1]]))
  } else {
    if (length(object@Code)>0 && all(nchar(object@Code)>0))
      return(length(object@Code))
  }
  NA
}

check_all_presets <- function(slick) {
  out <- list()

  if (!is_empty(slick@OMs))
    out$OMs <- check_Preset_OMs(slick@OMs)

  flat_classes <- c('MPs', 'Boxplot', 'Kobe', 'Quilt', 'Spider', 'Timeseries', 'Tradeoff')
  for (cl in flat_classes) {
    obj <- slot(slick, cl)
    if (is_empty(obj)) next
    if (cl=='MPs') {
      max_len <- max(getNMPs(obj@Code), getNMPs(obj@Label), getNMPs(obj@Description))
    } else {
      max_len <- nPI_count(obj)
    }
    if (!is.na(max_len) && max_len>0)
      out[[cl]] <- check_Preset(obj@Preset, max_len)
  }
  out
}

check_slick_file <- function(slick) {
  if (!inherits(slick, 'Slick') &  (!inherits(slick, 'SLICK')) & (!inherits(slick, 'Slick_old'))) {
    shinyalert::shinyalert('Incorrect File Type',
                           'The loaded file is not a Slick object',
                           type='error')
    return(NULL)
  }

  slick <- tryCatch(Update(slick), error=function(e) e)
  if (inherits(slick, 'error'))
    return(invalid_slick_alert(slick))

  check <- tryCatch(Check(slick), error=function(e) e)
  if (inherits(check, 'error'))
    return(invalid_slick_alert(check))

  if (length(unlist(check@errors))>0)
    return(invalid_slick_alert(detail=format_check_errors(check@errors)))

  preset_errors <- tryCatch(check_all_presets(slick), error=function(e) e)
  if (inherits(preset_errors, 'error'))
    return(invalid_slick_alert(preset_errors))
  if (length(unlist(preset_errors))>0)
    return(invalid_slick_alert(detail=format_check_errors(preset_errors)))

  slick <- tryCatch({
    if (any(nchar(slick@MPs@Color)<2)) {
      nMPs <- length(slick@MPs@Code)
      Color(slick@MPs) <- default_mp_colors(nMPs)
    }
    slick
  }, error=function(e) invalid_slick_alert(e))

  slick
}

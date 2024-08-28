get_language <- function(value, lang) {
  if (is.null(lang))
    return(value)

  if (inherits(value, 'data.frame')) {
    return(value)
  }
  if (inherits(value, 'list')) {
    if (is.null(names(value)))
      return(value)
    if (!lang %in% names(value)) {
      warning(lang, ' not found. Using default', call.=FALSE)
      return(value[[1]])
    }
    return(value[[lang]])
  }
  value
}


na_if_empty <- function(val) {
  if (length(val)<1) return(NA)
  val
}

check_slot_class <- function(object, slot, value) {
  cl_obj <- class(slot(object, slot))
  cl_val <- class(value)
  if (!cl_val %in% cl_obj)
    stop('`', slot, '` must be class `', cl_obj, '`', call.=FALSE)
  value
}

use_ifnot_NULL <- function(slot, value, object) {
  if (!is.null(value)) {
    slot(object, slot) <- value
  }
  slot(object, slot)
}


roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

check_assign_dataframe <- function(object, names, value) {
  chk <- names %in% colnames(value)
  missing <- names[!chk]
  if (length(missing)>0)
    stop(paste('`value` must be a data.frame with column names:',
               paste(names, collapse=', ')), call. = FALSE)
  for (nm in names) {
    slot(object, nm) <- value[[nm]]
  }
  object
}

default_mp_colors <- function(nMP) {
  if (!requireNamespace('colorspace', quietly = TRUE)) {
    warning('package `colorspace` required')
    cols <- grDevices::colors()
    return(cols[sample(length(cols), nMP)])
  }
  colorspace::qualitative_hcl(nMP, 'Dark2')
}


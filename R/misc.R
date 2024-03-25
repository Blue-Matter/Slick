

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

#' FilterSlick
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
FilterSlick <- function(slick,
                        selected_MPs=NULL,
                        selected_OMs=NULL,
                        selected_PMs=NULL,
                        fun='Boxplot') {

  if (is.null(slick)) {
    return(slick)
  }

  object <- get(fun)(slick)

  dd <- dim(Value(object))
  ll <- length(dd)

  # OMs
  if (length(selected_OMs)>0) {
    if (ll==3) {
      if (length(selected_OMs)<=dd[1])
        Value(object) <- Value(object)[selected_OMs,,,drop=FALSE]
    }
    if (ll==4) {
      if (length(selected_OMs)<=dd[2])
        Value(object) <- Value(object)[,selected_OMs,,, drop=FALSE]
    }
    if (ll==5) {
      if (length(selected_OMs)<=dd[2])
        Value(object) <- Value(object)[,selected_OMs,,,, drop=FALSE]
    }
  }

  # MPs
  if (length(selected_MPs)>0) {
    metadata <- Metadata(MPs(slick))
    if (ll==3) {
      if (length(selected_MPs)<=dd[2]) {
        Value(object) <- Value(object)[,selected_MPs,,drop=FALSE]
        Metadata(MPs(slick)) <- metadata[selected_MPs,]
      }
    }
    if (ll==4) {
      if (length(selected_MPs)<=dd[3]) {
        Metadata(MPs(slick)) <- metadata[selected_MPs,]
        Value(object) <- Value(object)[,,selected_MPs,, drop=FALSE]
      }
    }
    if (ll==5) {
      if (length(selected_MPs)<=dd[3]) {
        Metadata(MPs(slick)) <- metadata[selected_MPs,]
        Value(object) <- Value(object)[,,selected_MPs,,, drop=FALSE]
      }
    }
  }

  # PMs
  if (length(selected_PMs)>0) {
    Metadata(object) <- Metadata(object)[selected_PMs, ]

    if (ll==3) {
      if (length(selected_PMs)<=dd[3])
        Value(object) <- Value(object)[,,selected_PMs,drop=FALSE]
    }
    if (ll==4) {
      if (length(selected_PMs)<=dd[4])
        Value(object) <- Value(object)[,,,selected_PMs, drop=FALSE]
    }
    if (ll==5) {
      if (length(selected_PMs)<=dd[4])
        Value(object) <- Value(object)[,,,selected_PMs,, drop=FALSE]
    }
  }

  if (fun=='Boxplot') Boxplot(slick) <- object
  if (fun=='Kobe') Kobe(slick) <- object
  if (fun=='Quilt') Quilt(slick) <- object
  if (fun=='Spider') Spider(slick) <- object
  if (fun=='Timeseries') Timeseries(slick) <- object
  if (fun=='Tradeoff') Tradeoff(slick) <- object

  slick
}

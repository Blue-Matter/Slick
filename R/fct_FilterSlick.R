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

  # golem::print_dev(paste('Filter Slick', fun))
  # object <<- object
  # selected_MPs <<- selected_MPs
  # selected_OMs <<- selected_OMs
  # selected_PMs <<- selected_PMs
  #
  # golem::print_dev(selected_MPs)
  # golem::print_dev(selected_OMs)
  # golem::print_dev(selected_PMs)

  # OMs
  if (!is.null(selected_OMs) &  length(selected_OMs)>0) {
    if (ll==3) {
      if (all(selected_OMs<=dd[1])) {
        Value(object) <- Value(object)[selected_OMs,,,drop=FALSE]
      }

    }
    if (ll==4) {
      if (all(selected_OMs<=dd[2]))
        Value(object) <- Value(object)[,selected_OMs,,, drop=FALSE]
    }
    if (ll==5) {
      if (all(selected_OMs<=dd[2]))
        Value(object) <- Value(object)[,selected_OMs,,,, drop=FALSE]
    }
  }

  # MPs
  if (!is.null(selected_MPs) & length(selected_MPs)>0) {
    metadata <- Metadata(MPs(slick))
    if (ll==3) {
      if (all(selected_MPs<=dd[2])) {
        Value(object) <- Value(object)[,selected_MPs,,drop=FALSE]
        Metadata(MPs(slick)) <- metadata[selected_MPs,]
      }
    }
    if (ll==4) {
      if (all(selected_MPs<=dd[3])) {
        Metadata(MPs(slick)) <- metadata[selected_MPs,]
        Value(object) <- Value(object)[,,selected_MPs,, drop=FALSE]
      }
    }
    if (ll==5) {
      if (all(selected_MPs<=dd[3])) {
        Metadata(MPs(slick)) <- metadata[selected_MPs,]
        Value(object) <- Value(object)[,,selected_MPs,,, drop=FALSE]
      }
    }
  }

  # PMs
  if (!is.null(selected_PMs) & length(selected_PMs)>0) {
    Metadata(object) <- Metadata(object)[selected_PMs, ]

    if (ll==3) {
      if (all(selected_PMs<=dd[3]))
        Value(object) <- Value(object)[,,selected_PMs,drop=FALSE]
    }
    if (ll==4) {
      if (all(selected_PMs<=dd[4]))
        Value(object) <- Value(object)[,,,selected_PMs, drop=FALSE]
    }
    if (ll==5) {
      if (all(selected_PMs<=dd[4]))
        Value(object) <- Value(object)[,,,selected_PMs,, drop=FALSE]
    }
  }

  if (fun=='Boxplot') Boxplot(slick) <- object
  if (fun=='Kobe') Kobe(slick) <- object
  if (fun=='Quilt') Quilt(slick) <- object
  if (fun=='Spider') Spider(slick) <- object
  if (fun=='Timeseries') Timeseries(slick) <- object
  if (fun=='Tradeoff') Tradeoff(slick) <- object

  # golem::print_dev('Done Filter Slick')
  slick
}

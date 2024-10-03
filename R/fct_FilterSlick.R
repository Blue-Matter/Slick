#' FilterSlick
#'
#' Filter a Slick Object
#'
#' Filter a Slick Object by management procedures (MPs),
#' operating models (OMs), and performance indicators (PIs) for a given plot
#'
#' @param slick An object of class `Slick`
#' @param MPs Numeric values of the MPs to keep. Default NULL keeps all MPs.
#' @param OMs Numeric values of the OMs to keep (rows of `OM@Design`). Default NULL keeps all OMs.
#' @param PIs Numeric values of the PIs in `plot` to keep. Default NULL keeps all PIs.
#' @param plot The plot to filter the PIs. One of: `Timeseries`, `Boxplot`, `Kobe`,
#' `Quilt`, `Spider`,  or`Tradeoff`
#'
#' @return A filtered Slick Object
#' @export
#' @examples
#' \dontrun{
#' slick <- Slick() # a completed slick object
#' boxplot_OM_1 <- FilterSlick(slick, OMs=1, plot='boxplot') #
#' }
#'
#'
FilterSlick <- function(slick=NULL,
                        MPs=NULL,
                        OMs=NULL,
                        PIs=NULL,
                        plot=NULL) {

  if (is.null(slick))
    return(NULL)

  if (is.null(plot))
    cli::cli_abort('Argument `plot` must be specified')

  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  plot <- match.arg(plot, choices=c('Timeseries', 'Boxplot', 'Kobe',
                            'Quilt', 'Spider', 'Tradeoff'))

  object <- get(plot)(slick)

  dim_value <- dim(Value(object))
  len_dim <- length(dim_value)

  nMPs <- length(slick@MPs@Code)
  nOMs <- nrow(slick@OMs@Design)
  nPIs <- length(object@Code)

  if (is.null(MPs)) {
    MPs <- 1:nMPs
  } else {
    MPs <- MPs[MPs %in% 1:nMPs]
  }

  if (is.null(OMs)) {
    OMs <- 1:nOMs
  } else {
    OMs <- OMs[OMs %in% 1:nOMs]
  }

  if (is.null(PIs)) {
    PIs <- 1:nPIs
  } else {
    PIs <- PIs[PIs %in% 1:nPIs]
  }


  # Filter OMs
  if (!is.null(OMs) &  length(OMs)>0) {
    if (len_dim==3) {
      if (all(OMs<=dim_value[1])) {
        Value(object) <- Value(object)[OMs,,,drop=FALSE]
      }

    }
    if (len_dim==4) {
      if (all(OMs<=dim_value[2]))
        Value(object) <- Value(object)[,OMs,,, drop=FALSE]
    }
    if (len_dim==5) {
      if (all(OMs<=dim_value[2]))
        Value(object) <- Value(object)[,OMs,,,, drop=FALSE]
    }

    slick@OMs@Design <- slick@OMs@Design[OMs,, drop=FALSE]
  }

  # Filter MPs
  if (!is.null(MPs) & length(MPs)>0) {
    metadata <- Metadata(MPs(slick))
    if (len_dim==3) {
      if (all(MPs<=dim_value[2])) {
        Value(object) <- Value(object)[,MPs,,drop=FALSE]
        Metadata(slick@MPs) <- metadata[MPs,]
      }
    }
    if (len_dim==4) {
      if (all(MPs<=dim_value[3])) {
        Metadata(slick@MPs) <- metadata[MPs,]
        Value(object) <- Value(object)[,,MPs,, drop=FALSE]
      }
    }
    if (len_dim==5) {
      if (all(MPs<=dim_value[3])) {
        Metadata(slick@MPs) <- metadata[MPs,]
        Value(object) <- Value(object)[,,MPs,,, drop=FALSE]
      }
    }
  }

  # Filter PIs
  if (!is.null(PIs) & length(PIs)>0) {
    Metadata(object) <- Metadata(object)[PIs, ]

    if (len_dim==3) {
      if (all(PIs<=dim_value[3]))
        Value(object) <- Value(object)[,,PIs,drop=FALSE]
    }
    if (len_dim==4) {
      if (all(PIs<=dim_value[4]))
        Value(object) <- Value(object)[,,,PIs, drop=FALSE]
    }
    if (len_dim==5) {
      if (all(PIs<=dim_value[4]))
        Value(object) <- Value(object)[,,,PIs,, drop=FALSE]
    }
  }

  slot(slick, plot) <- object
  slick
}

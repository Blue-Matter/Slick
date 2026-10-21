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
#' @examplesIf interactive()
#' slick <- Slick() # a completed slick object
#' boxplot_OM_1 <- FilterSlick(slick, OMs=1, plot='boxplot')
#'
#'
#'
FilterSlick <- function(slick=NULL,
                        MPs=NULL,
                        OMs=NULL,
                        PIs=NULL,
                        plot=NULL) {

  if (is.null(slick))
    return(NULL)

  slick <- Update(slick)

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
    MPs <- MPs[!is.na(MPs) & MPs>=1 & MPs<=nMPs]
    if (length(MPs)<1)
      MPs <- 1:nMPs
  }

  if (is.null(OMs)) {
    OMs <- 1:nOMs
  } else {
    OMs <- OMs[!is.na(OMs) & OMs>=1 & OMs<=nOMs]
    if (length(OMs)<1)
      OMs <- 1:nOMs
  }

  if (is.null(PIs)) {
    PIs <- 1:nPIs
  } else {
    PIs <- PIs[!is.na(PIs) & PIs>=1 & PIs<=nPIs]
    if (length(PIs)<1)
      PIs <- 1:nPIs
  }

  # Filter OMs
  if (len_dim==3) {
    Value(object) <- Value(object)[OMs,,,drop=FALSE]
  }
  if (len_dim==4) {
    Value(object) <- Value(object)[,OMs,,, drop=FALSE]
  }
  if (len_dim==5) {
    Value(object) <- Value(object)[,OMs,,,, drop=FALSE]
  }
  slick@OMs@Design <- slick@OMs@Design[OMs,, drop=FALSE]

  # Filter MPs
  metadata <- Metadata(MPs(slick))
  if (len_dim==3) {
    object@Value <- Value(object)[,MPs,,drop=FALSE]
  }
  if (len_dim==4) {
    object@Value <- Value(object)[,,MPs,, drop=FALSE]
  }
  if (len_dim==5) {
    object@Value <- Value(object)[,,MPs,,, drop=FALSE]
  }
  Metadata(slick@MPs) <- metadata[MPs,]

  # Filter PIs
  if (len_dim==3) {
    object@Value <- Value(object)[,,PIs,drop=FALSE]
  }
  if (len_dim==4) {
    object@Value <- Value(object)[,,,PIs, drop=FALSE]
  }
  if (len_dim==5) {
    object@Value <- Value(object)[,,,PIs,, drop=FALSE]
  }
  if ('MinValue' %in% slotNames(object)) {
    object@MinValue <- object@MinValue[PIs]
    object@MaxValue <- object@MaxValue[PIs]
  }

  object@Code <- object@Code[PIs]
  object@Label <- object@Label[PIs]
  object@Description <- object@Description[PIs]

  slot(slick, plot) <- object
  slick
}


colorRampAlpha <- function(..., n, alpha) {
  colors <- grDevices::colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}

#' Plot `Quilt`
#'
#' Create a Quilt plot (unless `shading==FALSE` in which case it's just a table)
#'
#' The columns are color shaded from light (lowest values) to dark (highest values).
#'
#' Colors are set in `Color(quilt)`.
#'
#' The color shading has 10 steps, from `MinValue(quilt)` to `MaxValue(quilt)` for each
#' Performance Indicator. If those values are missing (`NA`) for a given PI, colors
#' are shaded from lowest to highest values. If `minmax==TRUE`, `MinValue(quilt)` and `MaxValue(quilt)`
#' are ignored.
#'
#' @param slick A [Slick-class()] object
#' @param MP_label Label to use for the MPs. Either `Code` or `Label`.
#' `Description` works as well, but you probably don't want to do that.
#' @param minmax Logical. Color shading from min to max values in each column?
#' If TRUE, ignores `MinValue(quilt)` and `MaxValue(quilt)`
#' @param shading Logical. Color shading for the columns?
#' @param kable Logical. Return a `kable` object?
#' @param signif Numeric Number of significant figures
#' @param alpha Numeric value. Transparency for color shading
#'
#' @seealso [Quilt-methods()], [Quilt-class()]
#' @return A `DT::datatable` or a `knitr::kable` object
#' @export
#'
plotQuilt <- function(slick, MP_label='Code', minmax=FALSE, shading=TRUE,
                      kable=FALSE, signif=2, alpha=0.5) {

  # slick <<- slick
  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  quilt <- Quilt(slick)
  chk <- Check(quilt)
  if (chk@empty)
    cli::cli_abort('`Quilt` in this `Slick` object is empty. Use  {.code Check(slick)}')

  if (!chk@complete)
    cli::cli_abort('`Quilt` in this `Slick` object is incomplete. Use  {.code Check(slick)}')


  # mean over OMs
  Values <- Value(quilt) |>
    apply(2:3, mean, na.rm=TRUE) |>
    signif(signif)

  if (all(is.na(Values))) {
    return(NULL)
  }

  nPI <- ncol(Values)

  mps <- MPs(slick)
  nMP <- length(mps@Code)
  MP_lab <- slot(mps, MP_label)
  rownames(Values) <- MP_lab

  PIMins <- as.numeric(apply(Values, 2, min))
  PIMaxs <- as.numeric(apply(Values, 2, max))

  if (minmax) {
    minVal <- PIMins
    maxVal <- PIMaxs
  } else {
    minVal <- MinValue(quilt)
    if (length(minVal)<nPI) {
      minVal <- rep(minVal, nPI)[1:nPI]
    }

    na.ind <- which(is.na(minVal))
    if (length(na.ind)>0) {
      minVal[na.ind] <- PIMins[na.ind]
    }

    maxVal <- MaxValue(quilt)
    if (length(maxVal)<nPI) {
      maxVal <- rep(maxVal, nPI)[1:nPI]
    }

    na.ind <- which(is.na(maxVal))
    if (length(na.ind)>0) {
      maxVal[na.ind] <- PIMaxs[na.ind]
    }
  }

  minVal[minVal>PIMins] <- PIMins[minVal<PIMins]
  maxVal[maxVal<PIMaxs] <- PIMaxs[maxVal<PIMaxs]

  metadata_pm <- Metadata(quilt)
  metadata_pm$MinValue <- minVal
  metadata_pm$MaxValue <- maxVal
  colnames(Values) <- metadata_pm$Code

  if (shading) {
    shading_list <- vector('list', nPI)
    for (i in 1:nPI) {
      shading_list[[i]]$cuts <- seq(minVal[i], maxVal[i], length.out=11)
      shading_list[[i]]$levels <- cut(Values[,i],
                                      breaks= shading_list[[i]]$cuts, include.lowest=TRUE) |> as.numeric()
      shading_list[[i]]$values <- rev(colorRampAlpha(Color(quilt),
                                                     n=length( shading_list[[i]]$cuts)+1,
                                                 alpha=alpha))
    }
  }

  if (kable) {
    if (!requireNamespace('flextable', quietly = TRUE))
      cli::cli_abort('package `flextable` required for this function')

    if (!requireNamespace('tibble', quietly = TRUE))
      cli::cli_abort('package `tibble` required for this function')

    df <- data.frame(Values)

    colnames(df) <- metadata_pm$Code
    table <- flextable::flextable(df |>
                                    tibble::rownames_to_column('MP'))

    if (shading) {
      for (i in 1:nPI) {
        table <- table |> flextable::bg(j=i+1, bg=shading_list[[i]]$values[shading_list[[i]]$levels ])
      }
    }
   return(table)
  }

  # DT::datatable

  extensions <- 'Buttons'
  options <-  list(dom = 'tB',
                   pageLength =100,
                   buttons=c('copy', 'csv'),
                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                   scrollX = TRUE
  )
  filter <-  list(
    position = 'top',
    clear = FALSE
  )

  if (shading) {
    table <- DT::datatable(Values, extensions = 'Buttons', class = list(stripe = FALSE),
                           options=options,
                           filter=filter,
                           selection = 'none')

  } else {
    table <- DT::datatable(Values, extensions = 'Buttons',
                           options=options,
                           filter=filter,
                           selection = 'none')
  }

  if (shading) {
    for (i in 1:nPI) {
      table <- table |>
        DT::formatStyle(i,
          backgroundColor = DT::styleInterval(cuts=shading_list[[i]]$cuts,
                                              values=shading_list[[i]]$values)
        )
    }
  }

  table <- table |>
    DT::formatStyle(column=0, fontWeight = 'bold')

  table

}

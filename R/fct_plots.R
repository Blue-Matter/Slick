
colorRampAlpha <- function(..., n, alpha) {
  colors <- grDevices::colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}



plotQuilt <- function(quilt, MP_labels=NULL, lang=NULL) {

  Values <- Value(quilt) |>
    apply(2:3, median) |>
    signif(3)
  if (all(is.na(Values))) {
    return(NULL)
  }

  if (is.null(MP_labels)) {
    warning('`MP_labels` not provided. Using default names')
    MP_labels <- paste('MP', 1:nrow(Values))
  }

  rownames(Values) <- MP_labels

  metadata_pm <- Metadata(quilt, lang)
  colnames(Values) <- metadata_pm$Code

  cols <- c(MinColor(quilt), MaxColor(quilt))

  outable <-  DT::datatable(Values, extensions = 'Buttons',
                            options = list(dom = 'tB',
                                           pageLength =100,
                                           buttons=c('copy', 'csv'),
                                           columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                           scrollX = TRUE
                            ),
                            filter = list(
                              position = 'top', clear = FALSE
                            ), selection = 'none')

  for (i in 1:ncol(Values)) {
    pm <- metadata_pm$Code[i]
    val_range <- range(Values[,i])

    minVal <- metadata_pm$MinValue[i]
    maxVal <- metadata_pm$MaxValue[i]

    cuts <- quantile(Values[,i], seq(minVal, maxVal, by=0.1)) |>
      as.numeric()
    values <- rev(colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5) )

    outable <- outable |>
      DT::formatStyle(
        pm,
        backgroundColor = DT::styleInterval(cuts=cuts,
                                            values=values)
      )
  }
  outable
}




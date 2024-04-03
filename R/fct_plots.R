
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


plotTradeoff <- function(quilt, mps, XPM, YPM) {
  quilt <<- quilt
  mps <<- mps
  XPM <<- XPM
  YPM <<- YPM

  MP_labels <- mps[['Label']]
  MP_color  <- mps[['Color']]

  Values <- Value(quilt) |>
    apply(2:3, median) |>
    signif(3)
  if (all(is.na(Values))) {
    return(NULL)
  }


  x_index <- match(XPM, Metadata(quilt)[['Code']])
  y_index <- match(YPM, Metadata(quilt)[['Code']])
  xlab <- Metadata(quilt)[['Label']][x_index]
  ylab <- Metadata(quilt)[['Label']][y_index]
  x_value <- Values[,x_index]
  y_value <- Values[, y_index]


  df <- data.frame(x=x_value, y=y_value,
                   MP=MP_labels,
                   Color=MP_color)
  df$MP <- factor(df$MP, ordered = TRUE, levels=unique(df$MP))

  ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=MP)) +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::coord_fixed() +
    ggplot2::expand_limits(x=0, y=0) +
    ggrepel::geom_text_repel(ggplot2::aes(label=MP), size=8) +
    ggplot2::scale_color_manual(values=df$Color) +
    ggplot2::guides(color='none') +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::theme(axis.title = ggplot2::element_text(size=16),
                   axis.text = ggplot2::element_text(size=12))

}





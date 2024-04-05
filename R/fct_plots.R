
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
    ggplot2::geom_point(size=2) +
    ggplot2::theme_classic() +
    ggplot2::expand_limits(x=0, y=0) +
    ggrepel::geom_text_repel(ggplot2::aes(label=MP), size=4) +
    ggplot2::scale_color_manual(values=df$Color) +
    ggplot2::guides(color='none') +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::theme(axis.title = ggplot2::element_text(size=16),
                   axis.text = ggplot2::element_text(size=12))

}


plotSpider <- function(spider) {

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
}


normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}

calcCoord <- function(vert, pm) {
  flipy <- flipx <- FALSE
  if (vert[1]< 0) {
    flipx <- TRUE
    vert[1] <- abs(vert[1])
  }

  if (vert[2] < 0) {
    flipy <- TRUE
    vert[2] <- abs(vert[2])
  }

  theta <- atan(vert[2]/vert[1])
  y <- sin(theta) * pm
  if (flipy) y <- -y
  x <- sqrt(pm^2 - y^2)
  if (flipx) x <- -x
  data.frame(x,y)
}

polyCoords <- function(n){
  # https://stackoverflow.com/a/29172340/2885462
  sq<-2*pi*(0:n)/n
  cbind(sin(sq),cos(sq))
}

get_luma <- function(rgb) {
  0.2126 * rgb[1] + 0.7152 * rgb[2] + 0.0722 * rgb[3]
}

Spiderplot <- function(slick, lang=NULL, relative_scale=FALSE, include_avg=TRUE) {

  mpMeta <- slick |> MPs() |>  Metadata(lang=lang)
  nMPs <- mpMeta |> nrow()
  mp_labels <- mpMeta$Label
  n.row <- ceiling(nMPs/4)
  n.col <- ceiling(nMPs/n.row)

  nPM <- slick |> Spider() |>
    Metadata() |> nrow()

  # plot controls
  fill.col <- '#f2f3f5'
  line.col <- 'white'
  lwd <- 3
  mplab.cex <- 2.2
  pm.avg.cex <- 2.5

  cols <- slick |> MPs() |>
    Metadata() |> dplyr::pull(Color)

  # median across OMs
  Values <- slick |> Spider() |>
    Value() |> apply(2:3, median, na.rm=TRUE)

  if(relative_scale) {
    # make all PMs relative to maximum and minimum values
    Values <- apply(Values, 2, normalize) * 100
    Values[!is.finite(Values)] <- 100
  }

  # average MP value across PMs
  if(include_avg) {
    mp.avg <- apply(Values, 1, mean, na.rm=TRUE)
    text_color <- rep('black', length(mp.avg))
    rgbvals <- grDevices::col2rgb(cols)
    luma <- apply(rgbvals, 2, get_luma)
    text_color[luma<50] <- 'lightgrey'
  }

  par(mfrow=c(n.row, n.col), mar=c(3,3,3,3), oma=rep(0,4))

  for (i in 1:nMPs) {
    # draw blank shape
    vertices <- polyCoords(nPM) * 100
    plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
    polygon(vertices, col=fill.col, border=NA)

    coords <- NULL
    for (j in 1:nPM) {
      pts <- calcCoord(vertices[j,], Values[i,j])
      coords <- rbind(coords, pts )
    }
    coords <- rbind(coords, coords[1,])
    polygon(coords, col=cols[i], border=cols[i])
    if (include_avg)
      text(0,0, round(mp.avg[i], 2), col=text_color[i], cex=mplab.cex, font=2)
    text(0, 100, mp_labels[i], xpd=NA, col=cols[i], cex=mplab.cex,
         pos=3)
  }
}




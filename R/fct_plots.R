
colorRampAlpha <- function(..., n, alpha) {
  colors <- grDevices::colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}


quilt_kable <- function(Values, metadata_pm, cols) {

  table <- flextable::flextable(data.frame(Values) |>
                                  tibble::rownames_to_column('MP'))


  for (i in 1:ncol(Values)) {
    minVal <- metadata_pm$MinValue[i]
    maxVal <- max(metadata_pm$MaxValue[i], Values[,i])

    cuts <- seq(minVal, maxVal, by=0.1*maxVal)
    levels <- cut(Values[,i], breaks=cuts, include.lowest=TRUE) |> as.numeric()
    colors <- rev(colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5) )

    table <-  table |>
      flextable::bg(j=i+1, bg=colors[levels])

  }

  table


}

quilt_DT <- function(Values, metadata_pm, cols) {

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

    cuts <- quantile(Values[,i], seq(0, 1, by=0.1)) |> as.numeric()
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

plotQuilt <- function(slick, MP_labels=NULL, lang=NULL, kable=FALSE) {

  quilt <-  Quilt(slick)
  # MP_labels <- MP_labels
  # lang <- lang
  # kable <- kable

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
  cols <- Colors(quilt)
  if (kable) {
    return(quilt_kable(Values, metadata_pm, cols))
  }
  quilt_DT(Values, metadata_pm, cols)
}


plotTradeoff <- function(slick, mps, XPM, YPM) {

  MP_labels <- mps[['Label']]
  MP_color  <- mps[['Color']]

  tradeoff <- slick |> Tradeoff()
  Values <- Value(tradeoff) |>
    apply(2:3, median) |>
    signif(3)
  if (all(is.na(Values))) {
    return(NULL)
  }

  x_index <- match(XPM, Metadata(tradeoff)[['Code']])
  y_index <- match(YPM, Metadata(tradeoff)[['Code']])
  xlab <- Metadata(tradeoff)[['Label']][x_index]
  ylab <- Metadata(tradeoff)[['Label']][y_index]
  x_value <- Values[,x_index]
  y_value <- Values[, y_index]

  if (length(x_value)<1) return(NULL)

  df <- data.frame(x=x_value, y=y_value,
                   MP=MP_labels,
                   Color=MP_color)

  xmax <- x_value |> pretty() |> max()
  ymax <- y_value |> pretty() |> max()

  df$MP <- factor(df$MP, ordered = TRUE, levels=unique(df$MP))

  ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=MP)) +
    ggplot2::geom_point(size=3) +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(x=c(0, xmax), y=c(0, ymax)) +
    ggrepel::geom_text_repel(ggplot2::aes(label=MP), size=6) +
    ggplot2::scale_color_manual(values=df$Color) +
    ggplot2::guides(color='none') +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::theme(axis.title = ggplot2::element_text(size=16),
                   axis.text = ggplot2::element_text(size=12))

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


pm_outline_plot <- function(n.PM,
                            fill.col=NA,
                            line.col='darkgrey',
                            pt.col='darkgrey',
                            text.col='white',
                            maxVal=100,
                            pt.cex=3) {
  par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(0,0,0,0))


  vertices <- polyCoords(n.PM) * maxVal
  plot(vertices, type="l", col=line.col, axes=FALSE, xlab="", ylab="", xpd=NA)
  if (!is.na(fill.col))
    polygon(vertices, col=fill.col, border=NA)

  lines(vertices*0.66, type="l", col=line.col, xpd=NA)
  lines(vertices*0.33, type="l", col=line.col, xpd=NA)
  for (i in 1:(nrow(vertices)-1)) {
    lines(x=c(0, vertices[i,1]),
          y=c(0, vertices[i,2]), col=line.col)
    points(x=vertices[i,1], y=vertices[i,2], pch=16, col=pt.col, cex=pt.cex, xpd=NA)
    text(x=vertices[i,1], y=vertices[i,2], col=text.col, LETTERS[i], xpd=NA)
  }
}

Spiderplot_single_OM <- function(Values_OM, MP_metadata, include_avg=TRUE) {

  nMP <- nrow(Values_OM)
  nPM <- ncol(Values_OM)
  # Calculate mean over MPs
  mean_over_MPs <- apply(Values_OM, 1, mean, na.rm=TRUE)
  MP_order <- rev(order(mean_over_MPs))

  vertices <- polyCoords(nPM) * 100

  mean_MP_Text <- paste0(round(mean_over_MPs,0), '%')

  highest_score <- Values_OM == max(Values_OM)

  if(include_avg) {
    text_color <- rep('black', length(mean_over_MPs))
    rgbvals <- grDevices::col2rgb(MP_metadata$Color)
    luma <- apply(rgbvals, 2, get_luma)
    text_color[luma<40] <- 'lightgrey'
  }

  # plot controls
  fill.col <- '#cccccc'
  lwd <- 1
  mplab.cex <- 2.5
  pt.cex <- 2
  pt.col <- 'darkred'

  par(mfrow=c(nMP, 1), oma=c(0,0,0,0), mar=c(1,1,1,1), bg=NA)

  # loop over MPs
  if (all(is.finite(vertices))) {
    for (r in MP_order) {
      plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
      polygon(vertices, col=fill.col, border=NA)

      coords <- NULL
      for (j in 1:nPM) {
        pts <- calcCoord(vertices[j,], Values_OM[r,j])
        coords <- rbind(coords, pts )
      }
      coords <- rbind(coords, coords[1,])
      polygon(coords, col=MP_metadata$Color[r], border=NA)

      for (j in 1:nPM) {
        if (!is.na(highest_score[r,j]) & highest_score[r,j])
          points(coords[j,], cex=pt.cex, col=pt.col, pch=16)
      }
      if (include_avg)
        text(0,0, mean_MP_Text[r],
             col=text_color[r], cex=mplab.cex, font=2)

    }
  }
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
    text_color[luma<40] <- 'lightgrey'
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


Spiderplot_all_MPs <- function(slick, relative_scale=FALSE) {

  # median across OMs
  metadata_MP <- slick |> MPs() |> Metadata()
  nMPs <- nrow(metadata_MP)

  spider <- slick |> Spider()
  Values <- spider |>
    Value() |> apply(2:3, median, na.rm=TRUE)

  metadata_PM <- spider |> Metadata()
  nPMs <- nrow(metadata_PM)

  if(relative_scale) {
    # make all PMs relative to maximum and minimum values
    Values <- apply(Values, 2, normalize) * 100
    Values[!is.finite(Values)] <- 100
  }

  # plot controls
  fill.col <- '#f2f3f5'
  line.col <- 'white'
  lwd <- 3

  vertices <- polyCoords(nPMs) * 100
  pm_outline_plot(nPMs, fill.col, 'black', 'darkgrey', 'white', 101, 5)

  for (i in 1:nMPs) {
    coords <- NULL
    for (j in 1:nPMs) {
      pts <- calcCoord(vertices[j,], Values[i,j])
      coords <- rbind(coords, pts )
    }
    coords <- rbind(coords, coords[1,])
    lines(coords, col=metadata_MP$Color[i], lwd=lwd)
  }


}


spiderplot_fun <- function(Det, MPkeep, Detkeep, SNkeep, Object, SwitchScale) {

  nSN <- nrow(Object$obj$OM$Design)
  nSNs <- sum(SNkeep$selected)

  nPMd <- dim(Object$obj$Perf$Det$Values)[3]
  nPMds <- sum(Detkeep$selected)

  nMP <- length(Object$obj$MP$Labels)
  nMPs <- sum(MPkeep$selected)




  # draw blank shape
  if(nPMds >2) {
    vertices <- polyCoords(nPMds) * 100
    par(mfrow=c(1,1), mar=c(3,3,3,3), oma=c(2,2,2,2))
    plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
    polygon(vertices, col=fill.col, border=NA)
    for (i in 1:nrow(vertices)) lines(c(0, vertices[i,1]), c(0, vertices[i,2]),
                                      col=line.col)

    # Calc median PM over OMs
    pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)
    pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]

    if (SwitchScale$relative) {
      # make all PMs relative to maximum and minimum values
      normalize <- function(x) {
        return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
      }
      if(nrow(pm)==1) {
        pm <- normalize(pm) * 100
      } else {
        pm <- apply(pm, 2, normalize) * 100
      }
    }

    # calculate coordinates for each MP
    # & plot lines for each mp

    cols <- Object$obj$Misc$Cols$MP[MPkeep$selected]
    Codes <- Object$obj$Perf$Det$Codes[Detkeep$selected]
    for (i in 1:nMPs) {
      coords <- NULL
      for (j in 1:nPMds) {
        pts <- calcCoord(vertices[j,], pm[i,j])
        coords <- rbind(coords, pts )
      }
      coords <- rbind(coords, coords[1,])
      lines(coords, col=cols[i], lwd=lwd)
    }

    for (j in 1:nPMds) {
      loc <- vertices[j,]
      loc <- round(loc,0) *1.05#* c(1.05, 0.95)
      if (loc[1]<0 & loc[2]<=0) pos <- 1
      if (loc[1]<0 & loc[2]>0) pos <- 2
      if (loc[1]>=0 & loc[2]>0) pos <- 3
      if (loc[1]>=0 & loc[2]<=0) pos <- 4

      txt <- Codes[j]
      text(loc[1],loc[2], pos=pos, txt, xpd=NA, col='black', cex=MPtxt)

    }
  }

}




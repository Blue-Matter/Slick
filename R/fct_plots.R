# slick <- readRDS('C:/users/user/downloads/Western Atlantic Skipjack.slick')


colorRampAlpha <- function(..., n, alpha) {
  colors <- grDevices::colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}




plotTradeoff <- function(slick, mps=NULL, XPM=NULL, YPM=NULL, lab_size=6, point_size=2) {

  if (is.null(mps)) {
    mps <- Metadata(MPs(slick))
  }
  MP_labels <- mps[['Label']]
  MP_color  <- mps[['Color']]

  tradeoff <- slick |> Tradeoff()
  Values <- Value(tradeoff) |>
    apply(2:3, mean) |>
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
  xmax <- max(c(xmax, 1))
  ymax <- y_value |> pretty() |> max()
  ymax <- max(c(ymax, 1))

  df$MP <- factor(df$MP, ordered = TRUE, levels=unique(df$MP))

  ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=MP)) +
    ggplot2::geom_point(size=point_size) +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(x=c(0, xmax), y=c(0, ymax)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggrepel::geom_text_repel(ggplot2::aes(label=MP), size=lab_size) +
    ggplot2::scale_color_manual(values=df$Color) +
    ggplot2::guides(color='none') +
    ggplot2::labs(x=xlab, y=ylab) +
    ggplot2::theme(axis.title = ggplot2::element_text(size=16),
                   axis.text = ggplot2::element_text(size=12))




}




#
# Spiderplot_single_OM <- function(Values_OM, MP_metadata, include_avg=TRUE) {
#
#   nMP <- nrow(Values_OM)
#   nPM <- ncol(Values_OM)
#   # Calculate mean over MPs
#   mean_over_MPs <- apply(Values_OM, 1, mean, na.rm=TRUE)
#   MP_order <- rev(order(mean_over_MPs))
#
#   vertices <- polyCoords(nPM) * 100
#
#   mean_MP_Text <- paste0(round(mean_over_MPs,0), '%')
#
#   highest_score <- Values_OM == max(Values_OM)
#
#   if(include_avg) {
#     text_color <- rep('#FF4500', length(mean_over_MPs))
#     # rgbvals <- grDevices::col2rgb(MP_metadata$Color)
#     # luma <- apply(rgbvals, 2, get_luma)
#     # text_color[luma<40] <- 'lightgrey'
#   }
#
#   # plot controls
#   fill.col <- '#cccccc'
#   lwd <- 1
#   mplab.cex <- 2.5
#   pt.cex <- 2
#   pt.col <- 'darkred'
#
#   par(mfrow=c(nMP, 1), oma=c(0,0,0,0), mar=c(1,1,1,1), bg=NA)
#
#   # loop over MPs
#   if (all(is.finite(vertices))) {
#     for (r in MP_order) {
#       plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
#       polygon(vertices, col=fill.col, border=NA)
#
#       coords <- NULL
#       for (j in 1:nPM) {
#         pts <- calcCoord(vertices[j,], Values_OM[r,j])
#         coords <- rbind(coords, pts )
#       }
#       coords <- rbind(coords, coords[1,])
#       polygon(coords, col=MP_metadata$Color[r], border=NA)
#
#       for (j in 1:nPM) {
#         if (!is.na(highest_score[r,j]) & highest_score[r,j])
#           points(coords[j,], cex=2, col=pt.col, pch=16)
#       }
#       if (include_avg)
#         text(0,0, mean_MP_Text[r],
#              col=text_color[r], cex=mplab.cex, font=2)
#
#     }
#   }
# }
#
#
#
# Spiderplot <- function(slick, lang=NULL, relative_scale=FALSE, include_avg=TRUE) {
#
#   mpMeta <- slick |> MPs() |>  Metadata(lang=lang)
#   nMPs <- mpMeta |> nrow()
#   mp_labels <- mpMeta$Label
#   n.row <- ceiling(nMPs/4)
#   n.col <- ceiling(nMPs/n.row)
#
#   nPM <- slick |> Spider() |>
#     Metadata() |> nrow()
#
#   # plot controls
#   fill.col <- '#f2f3f5'
#   line.col <- 'white'
#   lwd <- 3
#   mplab.cex <- 2.2
#   pm.avg.cex <- 2.5
#
#   cols <- slick |> MPs() |>
#     Metadata() |> dplyr::pull(Color)
#
#   # mean across OMs
#   Values <- slick |> Spider() |>
#     Value() |> apply(2:3, mean, na.rm=TRUE)
#
#   if(relative_scale) {
#     # make all PMs relative to maximum and minimum values
#     Values <- apply(Values, 2, normalize) * 100
#     Values[!is.finite(Values)] <- 100
#   }
#
#   # average MP value across PMs
#   if(include_avg) {
#     mp.avg <- apply(Values, 1, mean, na.rm=TRUE)
#     text_color <- rep('#FF4500', length(mp.avg))
#     # rgbvals <- grDevices::col2rgb(cols)
#     # luma <- apply(rgbvals, 2, get_luma)
#     # text_color[luma<40] <- 'lightgrey'
#   }
#
#   par(mfrow=c(n.row, n.col), mar=c(3,3,3,3), oma=rep(0,4))
#
#
#   for (i in 1:nMPs) {
#     # draw blank shape
#     vertices <- polyCoords(nPM) * 100
#     plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
#     polygon(vertices, col=fill.col, border=NA)
#
#     coords <- NULL
#     for (j in 1:nPM) {
#       pts <- calcCoord(vertices[j,], Values[i,j])
#       coords <- rbind(coords, pts )
#     }
#     coords <- rbind(coords, coords[1,])
#     polygon(coords, col=cols[i], border=cols[i])
#     if (include_avg)
#       text(0,0, round(mp.avg[i], 2), col=text_color[i], cex=mplab.cex, font=2)
#     text(0, 100, mp_labels[i], xpd=NA, col=cols[i], cex=mplab.cex,
#          pos=3)
#   }
# }
#
#
# Spiderplot_all_MPs <- function(slick, relative_scale=FALSE) {
#
#   # mean across OMs
#   metadata_MP <- slick |> MPs() |> Metadata()
#   nMPs <- nrow(metadata_MP)
#
#   spider <- slick |> Spider()
#   Values <- spider |>
#     Value() |> apply(2:3, mean, na.rm=TRUE)
#
#   metadata_PM <- spider |> Metadata()
#   nPMs <- nrow(metadata_PM)
#
#   if(relative_scale) {
#     # make all PMs relative to maximum and minimum values
#     Values <- apply(Values, 2, normalize) * 100
#     Values[!is.finite(Values)] <- 100
#   }
#
#   # plot controls
#   fill.col <- '#f2f3f5'
#   line.col <- 'white'
#   lwd <- 3
#
#   vertices <- polyCoords(nPMs) * 100
#   pm_outline_plot(nPMs, fill.col, 'black', 'darkgrey', 'white', 101, 5)
#
#   for (i in 1:nMPs) {
#     coords <- NULL
#     for (j in 1:nPMs) {
#       pts <- calcCoord(vertices[j,], Values[i,j])
#       coords <- rbind(coords, pts )
#     }
#     coords <- rbind(coords, coords[1,])
#     lines(coords, col=metadata_MP$Color[i], lwd=lwd)
#   }
#
#
# }
#
#
# spiderplot_fun <- function(Det, MPkeep, Detkeep, SNkeep, Object, SwitchScale) {
#
#   nSN <- nrow(Object$obj$OM$Design)
#   nSNs <- sum(SNkeep$selected)
#
#   nPMd <- dim(Object$obj$Perf$Det$Values)[3]
#   nPMds <- sum(Detkeep$selected)
#
#   nMP <- length(Object$obj$MP$Labels)
#   nMPs <- sum(MPkeep$selected)
#
#
#
#
#   # draw blank shape
#   if(nPMds >2) {
#     vertices <- polyCoords(nPMds) * 100
#     par(mfrow=c(1,1), mar=c(3,3,3,3), oma=c(2,2,2,2))
#     plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
#     polygon(vertices, col=fill.col, border=NA)
#     for (i in 1:nrow(vertices)) lines(c(0, vertices[i,1]), c(0, vertices[i,2]),
#                                       col=line.col)
#
#     # Calc mean PM over OMs
#     pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), mean, na.rm=TRUE)
#     pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]
#
#     if (SwitchScale$relative) {
#       # make all PMs relative to maximum and minimum values
#       normalize <- function(x) {
#         return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
#       }
#       if(nrow(pm)==1) {
#         pm <- normalize(pm) * 100
#       } else {
#         pm <- apply(pm, 2, normalize) * 100
#       }
#     }
#
#     # calculate coordinates for each MP
#     # & plot lines for each mp
#
#     cols <- Object$obj$Misc$Cols$MP[MPkeep$selected]
#     Codes <- Object$obj$Perf$Det$Codes[Detkeep$selected]
#     for (i in 1:nMPs) {
#       coords <- NULL
#       for (j in 1:nPMds) {
#         pts <- calcCoord(vertices[j,], pm[i,j])
#         coords <- rbind(coords, pts )
#       }
#       coords <- rbind(coords, coords[1,])
#       lines(coords, col=cols[i], lwd=lwd)
#     }
#
#     for (j in 1:nPMds) {
#       loc <- vertices[j,]
#       loc <- round(loc,0) *1.05#* c(1.05, 0.95)
#       if (loc[1]<0 & loc[2]<=0) pos <- 1
#       if (loc[1]<0 & loc[2]>0) pos <- 2
#       if (loc[1]>=0 & loc[2]>0) pos <- 3
#       if (loc[1]>=0 & loc[2]<=0) pos <- 4
#
#       txt <- Codes[j]
#       text(loc[1],loc[2], pos=pos, txt, xpd=NA, col='black', cex=MPtxt)
#
#     }
#   }
#
# }




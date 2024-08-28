#' Kobe_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
Kobe_plot <- function(slick, xvar=1, yvar=2,
                      ts=NA,
                      xmax=2, ymax=2,
                      inc_line=TRUE,
                      percentile=0.9) {

  if (is.null(percentile)) {
    quants <- NULL
  } else {
    quants <- c((1- percentile)/2, 1-(1- percentile)/2)
  }


  kobe <- Kobe(slick)
  metadata <- Metadata(kobe)
  times <- Time(kobe)

  targets <- Target(kobe)
  if (length(targets)<2)
    targets <- rep(targets, 2)
  x_targ <- targets[xvar]
  y_targ <- targets[yvar]

  # currently hard-coded to 1
  x_targ <- 1 #targets[xvar]
  y_targ <- 1 # targets[yvar]

  mp_metadata <- slick |> MPs() |> Metadata()
  mp_names <- factor(mp_metadata$Code, ordered = TRUE, levels=mp_metadata$Code)
  values <- kobe |> Value()
  dd <- dim(values)
  nsim <- dd[1]
  nOM <- dd[2]
  nMP <- dd[3]
  nvars <- dd[4]
  nyears <- dd[5]
  if (is.na(ts))
    ts <- nyears

  # background colors
  BL <- data.frame(x=c(0,x_targ,x_targ,0),
                   y=c(0,0,y_targ,y_targ), lab="BL",
                   color="#F8DC7A")

  TL <- data.frame(x=c(0,x_targ,x_targ,0),
                   y=c(y_targ,y_targ,ymax,ymax), lab="TL",
                   color="#D8775D")
  TR <- data.frame(x=c(x_targ, xmax, xmax, x_targ),
                   y=c(y_targ,y_targ, ymax,ymax), lab="TR",
                   color='#FDBD56')
  BR <- data.frame(x=c(x_targ, xmax, xmax, x_targ),
                   y=c(0,0, y_targ, y_targ), lab="BR",
                   color='#67C18B')
  bgDF <- rbind(BL, TL, TR, BR)
  bgDF$lab <- factor(bgDF$lab, ordered=TRUE, levels=unique(bgDF$lab))

  bgCols <- c("#F8DC7A", "#D8775D", '#FDBD56', '#67C18B')

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data=bgDF, ggplot2::aes(x=x, y=y, fill=lab), alpha=0.75) +
    ggplot2::scale_fill_manual(values=bgCols) +
    ggplot2::guides(fill="none", label="none") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_classic() +
    ggplot2::labs(x=metadata$Label[xvar],
                  y=metadata$Label[yvar])

  mean_over_OM_x <- apply(values[,,,xvar,, drop=FALSE], c(1,3,4,5), mean, na.rm=TRUE)
  mean_over_OM_y <- apply(values[,,,yvar,, drop=FALSE], c(1,3,4,5), mean, na.rm=TRUE)

  df <- data.frame(x=as.vector(apply(mean_over_OM_x, c(2,4), median, na.rm=TRUE)),
                   y=as.vector(apply(mean_over_OM_y, c(2,4), median, na.rm=TRUE)),
                   MP=mp_names,
                   time=rep(times, each=nMP))

  df$x[df$x > xmax ] <- xmax
  df$y[df$y > ymax ] <- ymax

  init_df <- df |> dplyr::filter(time==min(time))
  cur_df <- df |> dplyr::filter(time==max(time))

  p <- p + ggplot2::geom_point(data=cur_df,
                               ggplot2::aes(x=x, y=y, color=MP),
                               size=4) +
    ggplot2::scale_color_manual(values=mp_metadata$Color) +
    ggrepel::geom_text_repel(data=cur_df,
                             ggplot2::aes(x=x, y=y, color=MP, label=MP),
                             size=8) +
    ggplot2::guides(color='none') +
    ggplot2::coord_cartesian(clip='off')

  # error bars
  if (!is.null(quants)) {
    final_ts <- which.max(times)

    XError <- data.frame(x=as.vector(apply(mean_over_OM_x, 2, quantile, quants, na.rm=TRUE)),
                         y=rep(cur_df$y, each=2),
                         MP=rep(cur_df$MP, each=2))

    YError <- data.frame(x=rep(cur_df$x,each=2),
                         y=as.vector(apply(mean_over_OM_y, 2, quantile, quants, na.rm=TRUE)),
                         MP=rep(cur_df$MP, each=2))

    XError$x[XError$x >xmax ] <- xmax
    YError$y[YError$y >ymax ] <- ymax


    p <- p +
      ggplot2::geom_line(data=XError, ggplot2::aes(x=x, y=y, group=MP),
                         color='white', linetype='dotted', linewidth=1) +
      ggplot2::geom_line(data=YError, ggplot2::aes(x=x, y=y, group=MP),
                         color='white', linetype='dotted', linewidth=1)
  }


  if (inc_line) {
    p <- p +
      ggplot2::geom_path(data=df, ggplot2::aes(x=x, y=y, color=MP)) +
      ggplot2::geom_point(data=cur_df,
                          ggplot2::aes(x=x, y=y, color=MP),
                          size=4) +
      ggplot2::geom_point(data=init_df,
                          ggplot2::aes(x=x, y=y, color=MP),
                          size=2)
  }
  p +  ggplot2::theme(
    text = ggplot2::element_text(size=20)
  )
}


Kobe_time_plot <- function(slick, mp=1, xvar=1, yvar=2) {
  kobe <- Kobe(slick)
  metadata <- Metadata(kobe)
  times <- Time(kobe)

  targets <- Target(kobe)
  if (length(targets)<2)
    targets <- rep(targets, 2)
  x_targ <- targets[xvar]
  y_targ <- targets[yvar]

  # hard coded for now
  x_targ <- 1 #targets[xvar]
  y_targ <- 1 # targets[yvar]

  values <- kobe |> Value()

  mean_over_OMs <- apply(values, c(1,3,4,5), mean, na.rm=TRUE)

  mp_metadata <- slick |> MPs() |> Metadata()
  mp_names <- mp_metadata$Label
  # make data.frame
  kobe_time_list <- list()
  for (ts in seq_along(times)) {
    bl <- mean(mean_over_OMs[,mp,xvar,ts] <= x_targ &  mean_over_OMs[,mp,yvar,ts] <= y_targ)
    br <- mean(mean_over_OMs[,mp,xvar,ts] > x_targ &  mean_over_OMs[,mp,yvar,ts] <= y_targ)
    tl <- mean(mean_over_OMs[,mp,xvar,ts] <= x_targ &  mean_over_OMs[,mp,yvar,ts] > y_targ)
    tr <- mean(mean_over_OMs[,mp,xvar,ts] > x_targ &  mean_over_OMs[,mp,yvar,ts] > y_targ)
    kobe_time_list[[ts]] <-  data.frame(x=times[ts], y=c(bl, br, tl, tr), quadrant=c('bl', 'br', 'tl', 'tr'),
                                        MP=mp_names[mp])
  }

  kobe_time_df <- do.call('rbind', kobe_time_list)
  kobe_time_df$quadrant <- factor(kobe_time_df$quadrant, ordered = TRUE,
                                  levels=c('br', 'tr', 'bl', 'tl'))

  cols <- c( '#67C18B',  '#FDBD56', "#F8DC7A", "#D8775D")

  p1 <- ggplot2::ggplot(kobe_time_df, ggplot2::aes(x=x, y=y, fill=quadrant)) +
    ggplot2::geom_bar(position="stack", stat="identity", width = 1) +
    ggplot2::scale_fill_manual(values=cols) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
    ggplot2::guides(fill='none') +
    ggplot2::expand_limits(y=1) +
    ggplot2::labs(x=names(Time(kobe))[1],
                  y='',
                  title=mp_names[mp]) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                   axis.title = ggplot2::element_text(size=14),
                   plot.title = ggplot2::element_text(size=16, face='bold'))

  p1

}



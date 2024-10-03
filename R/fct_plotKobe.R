
#' Plot `Kobe`
#'
#' Plots a Kobe plot for a given projection year, or a Kobe Time plot.
#'
#' @param slick A [Slick-class()] object
#' @param xPI Numeric value specifying the performance indicator for the x-axis
#' @param yPI Numeric value specifying the performance indicator for the y-axis
#' @param TS Numeric value specifying the projection time-step for the Kobe plot. Default is NA
#' which means the terminal projection time-step
#' @param Time Logical. Kobe Time plot?
#' @param BLcol Color for the bottom left quadrant
#' @param TLcol Color for the top left quadrant
#' @param TRcol Color for the top  right quadrant
#' @param BRcol Color for the bottom right quadrant
#' @param axis_label Label to use for the axes. Either `Code` or `Label`.
#' `Description` works as well, but you probably don't want to do that.
#' @param percentile Numeric value specifying the percentile for the x and y percentile bars.
#' Use NULL to remove percentile lines.
#' @param axis.text.size Font size for axis text
#' @param axis.title.size Font size for axis title
#' @param strip.text.size Font size for facet strip text
#' @param strip.text.color Color for facet strip text
#' @param incMP_label Logical. Include MP labels?
#' @param mp.text.size Font size for MP labels
#' @param mp.point.size Point size for MP labels
#' @param mp.init.point.size Point size for start of trajectory. If `hist_traj==TRUE`
#' @param xmax Maximum value for the x-axis. Values greater than `xmax` will be shown at `xmax`
#' @param ymax Maximum value for the yx-axis. Values greater than `ymax` will be shown at `ymax`
#' @param hist_traj Logical. Plot the historical trajectories?
#' @param ncol Numeric. Number of columns for Kobe Time
#' @param lang Optional. Language (if supported in Slick Object). Either 'en', 'es', 'fr'
#' @param MP_label Label to use for the MPs. Either `Code` or `Label`.
#' `Description` works as well, but you probably don't want to do that.
#' @seealso [Kobe()], [Kobe-class()]
#' @return A `ggplot2` object
#' @export
#'
plotKobe <- function(slick,
                     xPI=1,
                     yPI=2,
                     TS=NA,
                     Time=FALSE,
                     BLcol="#F8DC7A",
                     TLcol="#D8775D",
                     TRcol='#FDBD56',
                     BRcol='#67C18B',
                     axis_label='Code',
                     percentile=0.9,
                     axis.text.size=14,
                     axis.title.size=16,
                     strip.text.size=16,
                     strip.text.color='#D6501C',
                     incMP_label=TRUE,
                     mp.text.size=7,
                     mp.point.size=4,
                     mp.init.point.size=2,
                     xmax=2,
                     ymax=2,
                     hist_traj=FALSE,
                     ncol=4,
                     lang='en',
                     MP_label='Code') {

  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  kobe <- Kobe(slick)
  chk <- Check(kobe)
  if (chk@empty)
    cli::cli_abort('`Kobe` in this `Slick` object is empty. Use  {.code Check(slick)}')

  if (!chk@complete)
    cli::cli_abort('`Kobe` in this `Slick` object is incomplete. Use  {.code Check(slick)}')


  metadata <- Metadata(kobe)
  times <- Time(kobe)
  timelab <- TimeLab(kobe, lang)


  axis_labels <- slot(kobe, axis_label)
  nPI <- length(axis_labels)

  if (xPI >nPI)
    cli::cli_abort('`xPI` is greater than the number of performance indicators')

  if (yPI >nPI)
    cli::cli_abort('`yPI` is greater than the number of performance indicators')


  targets <- Target(kobe)
  if (length(targets)<1) {
    cli::cli_alert_info('`Target` not specified. Defaulting to 1')
    targets <- rep(1, 2)
  }
  if (length(targets)<2)
    targets <- rep(targets, 2)
  x_targ <- targets[xPI]
  y_targ <- targets[yPI]

  if (is.null(percentile)) {
    quants <- NULL
  } else {
    quants <- c((1- percentile)/2, 1-(1- percentile)/2)
  }

  MP_info <- get_MP_info(slick, MP_label, nMP)
  MP_lab <- MP_info$MP_lab
  MP_colors <- MP_info$MP_colors

  values <- Value(kobe)
  dd <- dim(values)
  nsim <- dd[1]
  nOM <- dd[2]
  nMP <- dd[3]
  nvars <- dd[4]
  nTS <- dd[5]
  if (is.na(TS))
    TS <- nTS

  mean_over_OMs <- apply(values, c(1,3,4,5), mean, na.rm=TRUE)

  if (Time) {
    bgCols <- c(BRcol, TRcol, BLcol, TLcol)
    kobe_time_list_mp <- list()
    for (mp in seq_along(MP_lab)) {
      kobe_time_list <- list()
      for (ts in seq_along(times)) {
        bl <- mean(mean_over_OMs[,mp,xPI,ts] <= x_targ &  mean_over_OMs[,mp,yPI,ts] <= y_targ)
        br <- mean(mean_over_OMs[,mp,xPI,ts] > x_targ &  mean_over_OMs[,mp,yPI,ts] <= y_targ)
        tl <- mean(mean_over_OMs[,mp,xPI,ts] <= x_targ &  mean_over_OMs[,mp,yPI,ts] > y_targ)
        tr <- mean(mean_over_OMs[,mp,xPI,ts] > x_targ &  mean_over_OMs[,mp,yPI,ts] > y_targ)
        kobe_time_list[[ts]] <-  data.frame(x=times[ts], y=c(bl, br, tl, tr), quadrant=c('bl', 'br', 'tl', 'tr'),
                                            MP=MP_lab[mp])
      }
      kobe_time_list_mp[[mp]] <- do.call('rbind', kobe_time_list)
    }

    kobe_time_df <- do.call('rbind', kobe_time_list_mp)
    kobe_time_df$quadrant <- factor(kobe_time_df$quadrant, ordered = TRUE,
                                    levels=c('br', 'tr', 'bl', 'tl'))

    p <- ggplot2::ggplot(kobe_time_df, ggplot2::aes(x=x, y=y, fill=quadrant)) +
      ggplot2::facet_wrap(~MP, ncol=ncol) +
      ggplot2::geom_bar(position="stack", stat="identity", width = 1) +
      ggplot2::scale_fill_manual(values=bgCols) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
      ggplot2::guides(fill='none') +
      ggplot2::expand_limits(y=c(0,1)) +
      ggplot2::labs(x=timelab,y='')



  } else {
    bgCols <- c(BLcol, TLcol, TRcol, BRcol)
    # background colors
    BL <- data.frame(x=c(0,x_targ,x_targ,0),
                     y=c(0,0,y_targ,y_targ), lab="BL",
                     color=BLcol)
    TL <- data.frame(x=c(0,x_targ,x_targ,0),
                     y=c(y_targ,y_targ,ymax,ymax), lab="TL",
                     color=TLcol)
    TR <- data.frame(x=c(x_targ, xmax, xmax, x_targ),
                     y=c(y_targ,y_targ, ymax,ymax), lab="TR",
                     color=TRcol)
    BR <- data.frame(x=c(x_targ, xmax, xmax, x_targ),
                     y=c(0,0, y_targ, y_targ), lab="BR",
                     color=BRcol)
    bgDF <- rbind(BL, TL, TR, BR)
    bgDF$lab <- factor(bgDF$lab, ordered=TRUE, levels=unique(bgDF$lab))

    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data=bgDF, ggplot2::aes(x=x, y=y, fill=lab), alpha=0.75) +
      ggplot2::scale_fill_manual(values=bgCols) +
      ggplot2::guides(fill="none", label="none") +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::theme_classic() +
      ggplot2::labs(x=axis_labels[xPI],
                    y=axis_labels[yPI])

    df <- data.frame(x=as.vector(apply(mean_over_OMs[,,xPI,, drop=FALSE], c(2,4), median, na.rm=TRUE)),
                     y=as.vector(apply(mean_over_OMs[,,yPI,, drop=FALSE], c(2,4), median, na.rm=TRUE)),
                     MP=MP_lab,
                     time=rep(times, each=nMP))

    df <- df |> dplyr::filter(time<=times[TS])

    df$x[df$x > xmax ] <- xmax
    df$y[df$y > ymax ] <- ymax

    init_df <- df |> dplyr::filter(time==min(time))
    cur_df <- df |> dplyr::filter(time==max(time))

    # error bars
    if (!is.null(quants)) {
      final_ts <- TS

      XError <- data.frame(x=as.vector(apply(mean_over_OMs[,,xPI,final_ts, drop=FALSE], 2, quantile, quants, na.rm=TRUE)),
                           y=rep(cur_df$y, each=2),
                           MP=rep(cur_df$MP, each=2))

      YError <- data.frame(x=rep(cur_df$x,each=2),
                           y=as.vector(apply(mean_over_OMs[,,yPI,final_ts, drop=FALSE], 2, quantile, quants, na.rm=TRUE)),
                           MP=rep(cur_df$MP, each=2))

      XError$x[XError$x >xmax ] <- xmax
      YError$y[YError$y >ymax ] <- ymax

      p <- p +
        ggplot2::geom_line(data=XError, ggplot2::aes(x=x, y=y, group=MP),
                           color='white', linetype='dotted', linewidth=1) +
        ggplot2::geom_line(data=YError, ggplot2::aes(x=x, y=y, group=MP),
                           color='white', linetype='dotted', linewidth=1)
    }



    # historical trajectory
    if (hist_traj) {
      p <- p +
        ggplot2::geom_path(data=df, ggplot2::aes(x=x, y=y, color=MP)) +
        ggplot2::geom_point(data=init_df,
                            ggplot2::aes(x=x, y=y, color=MP),
                            size=mp.init.point.size)
    }

    p <- p + ggplot2::geom_point(data=cur_df,
                                 ggplot2::aes(x=x, y=y, color=MP),
                                 size=mp.point.size) +
      ggplot2::scale_color_manual(values=MP_colors) +
      ggplot2::coord_cartesian(clip='off')

    if (incMP_label) {
      p <- p + ggrepel::geom_text_repel(data=cur_df,
                                        ggplot2::aes(x=x, y=y, color=MP, label=MP),
                                        size=mp.text.size)
    }
  }

  p +
    ggplot2::guides(color='none') +
    ggplot2::theme(axis.text = ggplot2::element_text(size=axis.text.size),
                   axis.title = ggplot2::element_text(size=axis.title.size),
                   strip.text = ggplot2::element_text(colour=strip.text.color,
                                                      size=strip.text.size, face='bold'),
                   strip.background = ggplot2::element_blank())

}

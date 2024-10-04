#' Plot `Timeseries`
#'
#' Plot the historical and projected values for a performance indicator.
#'
#' If `byOM==FALSE` the results are shown as the mean across operating models.
#'
#' @param slick A [Slick-class()] object
#' @param PI A numeric value specifying the performance indicator to plot
#' @param byMP Logical. Facet by MP? Defaults to FALSE, where all MPs are shown on the same plot
#' @param byOM Logical. Facet by OM?  Defaults to FALSE where values are calculated as mean across OMs
#' @param includeHist Logical. Include the historical period in the projections?
#' @param ncol Numeric. Number of columns if faceting by MP or OM
#' @param col_line Color for the median line (historical)
#' @param includeQuants Logical. Include quantile shading for the projections?
#' @param includeLabels Logical. Include MP labels?
#' @param fill_ribbon1 Fill color for the inner ribbon
#' @param col_ribbon1  Color of the line for inner ribbon
#' @param quants1 Quantiles for the inner ribbon. Numeric length 2
#' @param alpha1 Alpha for the colored MPs inner shading
#' @param fill_ribbon2 Fill color for the outer ribbon
#' @param col_ribbon2 Color of the line for outer ribbon
#' @param linetype_ribbon2 Line type for outer ribbon
#' @param quants2 Quantiles for the outer ribbon. Numeric length 2.
#' @param alpha2 Alpha for the colored MPs outer shading
#' @param MP_label Label to use for the MPs. Either `Code` or `Label`.
#' `Description` works as well, but you probably don't want to do that.
#' @param size.title Numeric length 1. Size for plot title
#' @param size.axis.title Numeric length 1. Size for axis title
#' @param size.axis.text Numeric length 1. Size for axis text
#' @param size.mp.label Numeric length 1. Size of MP labels. Set to NULL for no MP labels
#' @param targ_color Color for the target line (if it exists in `Target(Timeseries(slick))`)
#' @param targ_name Label for the target line
#' @param lim_color Color for the limit line (if it exists in `Limit(Timeseries(slick))`)
#' @param lim_name Label for the limit line
#' @param inc_y_label Include the label for the y-axis?
#' @param sims Optional. Numeric values indicating the simulations to include. Defaults
#' to all.
#' @param lang Optional. Language (if supported in Slick Object). Either 'en', 'es', 'fr'
#'
#' @seealso [Timeseries()], [Timeseries-class()]
#' @return A `ggplot2` object
#' @example inst/examples/Timeseries.R
#' @export
#'
plotTimeseries <- function(slick,
                           PI=1,
                           byMP=FALSE,
                           byOM=FALSE,
                           includeHist=TRUE,
                           ncol=4,
                           col_line='darkgray',
                           includeQuants=TRUE,
                           includeLabels=TRUE,
                           fill_ribbon1='#ededed',
                           col_ribbon1='#ededed',
                           quants1=c(0.25, 0.75),
                           alpha1=0.3,
                           fill_ribbon2='white',
                           col_ribbon2='#c9c9c9',
                           linetype_ribbon2='dashed',
                           quants2=c(0.1, 0.9),
                           alpha2=0.1,
                           MP_label='Code',
                           size.title=18,
                           size.axis.title=18,
                           size.axis.text=16,
                           size.mp.label=6,
                           targ_color='green',
                           targ_name='Target',
                           lim_color='red',
                           lim_name='Limit',
                           inc_y_label=TRUE,
                           sims=NULL,
                           lang='en') {

  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  timeseries <- Timeseries(slick)
  chk <- Check(timeseries)
  if (chk@empty)
    cli::cli_abort('`Timeseries` in this `Slick` object is empty. Use  {.code Check(slick)}')

  if (!chk@complete)
    cli::cli_abort('`Timeseries` in this `Slick` object is incomplete. Use  {.code Check(slick)}')

  values <- Value(timeseries)
  times <- Time(timeseries)
  time_lab <- TimeLab(timeseries, lang=lang)
  time_now <- TimeNow(timeseries)
  target <- Target(timeseries)
  limit <- Limit(timeseries)
  metadata <- Metadata(timeseries)
  PI_lab <- metadata$Label[PI]

  if (PI>nrow(metadata)) {
    cli::cli_abort('`PI` is greater than the number of performance indicators in `Timeseries(slick)`')
  }

  dd <- dim(values)
  nsim <- dd[1]
  nOM <- dd[2]
  nMP <- dd[3]
  nPI <- dd[4]
  nTS <- dd[5]

  MP_info <- get_MP_info(slick, MP_label, nMP)
  MP_lab <- MP_info$MP_lab
  MP_colors <- MP_info$MP_colors

  if (is.null(sims)) {
    sims <- 1:dim(values)[1]
  }

  oms <- 1:nOM
  om_names <- get_OM_labels(slick, nOM)

  hist.yr.ind <- which(times<=time_now) |> max()
  hist.yrs <-  times[1:hist.yr.ind]
  proj.yr.ind <- (hist.yr.ind+1):length(times)
  proj.yrs <- times[proj.yr.ind]

  p <- ggplot2::ggplot() +
    ggplot2::theme_bw()

  if (includeHist) {
    # Historical Period
    if (byOM) {
      mean.hist <- values[,oms,1, PI,1:hist.yr.ind, drop=FALSE]
      med.hist <- apply(mean.hist, c(2,5), median, na.rm=TRUE) # median over simulations
      quant.1.hist <- apply(mean.hist, c(2,5), quantile, probs=quants1, na.rm=TRUE)
      quant.2.hist <- apply(mean.hist, c(2,5), quantile, probs=quants2, na.rm=TRUE)

      Hist_df <- data.frame(OM=om_names,
                            x=rep(hist.yrs, each=nOM),
                            Median=as.vector(med.hist),
                            Lower1=as.vector(quant.1.hist[1,,]),
                            Upper1=as.vector(quant.1.hist[2,,]),
                            Lower2=as.vector(quant.2.hist[1,,]),
                            Upper2=as.vector(quant.2.hist[2,,]))
      Hist_df$OM <- factor(Hist_df$OM, levels=om_names, ordered = TRUE)

    } else {
      mean.hist <- apply(values[,oms,1, PI,1:hist.yr.ind, drop=FALSE], c(1,4,5), mean, na.rm=TRUE) # mean over OMs
      med.hist <- apply(mean.hist, 3, median, na.rm=TRUE) # median over simulations
      quant.1.hist <- apply(mean.hist, 3, quantile, probs=quants1, na.rm=TRUE)
      quant.2.hist <- apply(mean.hist, 3, quantile, probs=quants2, na.rm=TRUE)

      Hist_df <- data.frame(x=hist.yrs,
                            Median=med.hist,
                            Lower1=quant.1.hist[1,],
                            Upper1=quant.1.hist[2,],
                            Lower2=quant.2.hist[1,],
                            Upper2=quant.2.hist[2,])

    }
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower2, ymax=Upper2), data=Hist_df,
                                  color=col_ribbon2, fill=fill_ribbon2, linetype=linetype_ribbon2) +
      ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower1, ymax=Upper1), data=Hist_df,
                           fill=fill_ribbon1, col=col_ribbon1) +
      ggplot2::geom_line(ggplot2::aes(x=x, y=Median), data=Hist_df,
                         color=col_line)

  }

  # Projection
  if (byOM) {
    mean.mps <- values[sims,oms,, PI,proj.yr.ind, drop=FALSE]
    med.mps <- apply(mean.mps, c(2,3,5), median, na.rm=TRUE)

    meddf <- data.frame(OM=om_names,
                        MP=rep(MP_lab, each=nOM),
                        x=rep(proj.yrs, each=nMP*nOM),
                        Median=as.vector(med.mps))


    meddf$OM <- factor(meddf$OM, levels=om_names, ordered = TRUE)

    quant.1.mp <- apply(mean.mps, c(2,3,5), quantile, probs=quants1, na.rm=TRUE)
    quant.2.mp <- apply(mean.mps, c(2,3,5), quantile, probs=quants2, na.rm=TRUE)

    quant_df <- data.frame(OM=om_names,
                           MP=rep(MP_lab, each=nOM),
                           x=rep(proj.yrs, each=nMP*nOM),
                           Lower1=as.vector(quant.1.mp[1,,,]),
                           Upper1=as.vector(quant.1.mp[2,,,]),
                           Lower2=as.vector(quant.2.mp[1,,,]),
                           Upper2=as.vector(quant.2.mp[2,,,]))

    quant_df$MP <- factor(quant_df$MP, ordered=TRUE, levels=MP_lab)
    quant_df$OM <- factor(quant_df$OM, levels=om_names, ordered = TRUE)

    # if (includeHist) {
    #   hist_last <- Hist_df |> dplyr::filter(x==max(x)) |>
    #     dplyr::select(x, Median, OM)
    #   hist_last <- replicate(nMP, hist_last, simplify = FALSE)
    #   hist_last <- do.call("rbind", hist_last)
    #
    #   hist_last$MP <- rep(unique(meddf$MP), each=nOM)
    #   meddf <- dplyr::bind_rows(hist_last, meddf)
    #
    # }

    if (includeQuants) {
      if (byMP) {
        p <- p +
          ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower2, ymax=Upper2), data=quant_df,
                               color=col_ribbon2, fill=fill_ribbon2, linetype=linetype_ribbon2) +
          ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower1, ymax=Upper1), data=quant_df,
                               fill=fill_ribbon1, col=col_ribbon1)
      } else {
        p <- p +
          ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower2, ymax=Upper2,fill=MP), data=quant_df,
                               alpha=alpha2) +
          ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower1, ymax=Upper1, fill=MP), alpha=alpha1,
                               data=quant_df)
      }
    }


    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=x, y=Median, color=MP), data=meddf) +
      ggplot2::scale_color_manual(values=MP_colors) +
      ggplot2::scale_fill_manual(values=MP_colors) +
      ggplot2::guides(color='none', fill='none')

  } else {

    # mean over OMs
    if (any(is.na(values[sims,oms, , PI,proj.yr.ind, drop=FALSE]))) {
      mean.mps <- apply(values[sims,oms,, PI,proj.yr.ind, drop=FALSE], c(1,3,5), mean, na.rm=TRUE)
    } else {
      mean.mps <- apply(values[sims,oms,, PI,proj.yr.ind, drop=FALSE], c(1,3,5), sum)/
        apply(values[sims,oms,, PI,proj.yr.ind, drop=FALSE], c(1,3,5), length)
    }
    med.mps <- apply(mean.mps, c(2,3), median, na.rm=TRUE)
    meddf <- data.frame(x=rep(proj.yrs, each=nMP),
                        MP=MP_lab,
                        Median=as.vector(med.mps))
    meddf$MP <- factor(meddf$MP, ordered=TRUE, levels=MP_lab)

    quant.1.mp <- apply(mean.mps, 2:3, quantile, probs=quants1, na.rm=TRUE)
    quant.2.mp <- apply(mean.mps, 2:3, quantile, probs=quants2, na.rm=TRUE)

    quant_df <- data.frame(MP=MP_lab,
                           x=rep(proj.yrs, each=length(MP_lab)),
                           Lower1=as.vector(quant.1.mp[1,,]),
                           Upper1=as.vector(quant.1.mp[2,,]),
                           Lower2=as.vector(quant.2.mp[1,,]),
                           Upper2=as.vector(quant.2.mp[2,,]))

    quant_df$MP <- factor(quant_df$MP, ordered=TRUE, levels=MP_lab)

    if (!byMP) {
      if (includeQuants)
        p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower2, ymax=Upper2, fill=MP), data=quant_df,
                             alpha=alpha2) +
        ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower1, ymax=Upper1, fill=MP), data=quant_df, alpha=alpha1)

      p <- p +
        ggplot2::geom_line(ggplot2::aes(x=x, y=Median, color=MP), data=meddf) +
        ggplot2::scale_color_manual(values=MP_colors) +
        ggplot2::scale_fill_manual(values=MP_colors) +
        ggplot2::guides(color='none', fill='none')

    } else {
      if (includeQuants)
        p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower2, ymax=Upper2,fill=MP), data=quant_df,
                             color=col_ribbon2, fill=fill_ribbon2, linetype=linetype_ribbon2) +
        ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower1, ymax=Upper1, fill=MP), data=quant_df,
                             fill=fill_ribbon1, col=col_ribbon1)
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x=x, y=Median, color=MP), data=meddf) +
        ggplot2::scale_color_manual(values=MP_colors) +
        ggplot2::guides(color='none')
    }


  }

  X <- MP <- NULL # CRAN checks
  if (!byMP & includeLabels) {
    tt <- data.frame(MP=MP_lab, X=sample(unique(meddf$x), nMP))

    tt <- dplyr::left_join(meddf, tt, by = dplyr::join_by(MP))

    meddf_last <- tt |> dplyr::group_by(MP) |>
      dplyr::filter(x==X)

    p <- p + ggrepel::geom_text_repel(data=meddf_last,
                                 ggplot2::aes(x=x, y=Median,
                                              label=MP, color=MP),
                                 size=size.mp.label,
                                 max.overlaps = Inf,
                                 point.padding = 0.2,
                                 nudge_x = .15,
                                 nudge_y = .5,
                                 segment.linetype = 6,
                                 segment.curvature = 0.1,
                                 arrow=grid::arrow(length = grid::unit(0.015, "npc"))
    )

  }



  # add target and limit lines if they exist
  if (!is.null(target)) {
    targ <- target[PI]
    if (!is.na(targ)) {
      x_loc <- times[1]
      if (!includeHist) {
        x_loc <- time_now+1
      }

      p <- p + ggplot2::geom_hline(yintercept = targ,
                                   color=targ_color,
                                   alpha=0.5,
                                   linetype=2) +
        ggrepel::geom_text_repel(data=data.frame(x=x_loc, y=targ),
                                 ggplot2::aes(x=x, y=y),
                                 label=targ_name, color=targ_color)
    }
  }
  if (!is.null(limit)) {
    lim <- limit[PI]
    if (!is.na(targ)) {
      x_loc <- times[1]
      if (!includeHist) {
        x_loc <- time_now+1
      }

      p <- p + ggplot2::geom_hline(yintercept = lim, color=lim_color,
                                   alpha=0.5,
                                   linetype=2) +
        ggrepel::geom_text_repel(data=data.frame(x=x_loc, y=lim),
                                 ggplot2::aes(x=x, y=y),
                                 label=lim_name, color=lim_color)
    }
  }



  if (byOM) {
    if (byMP) {
      p <- p + ggplot2::facet_grid(MP~OM)
    } else {
      p <- p + ggplot2::facet_wrap(~OM, ncol=ncol)
    }
  } else {
    if (byMP) {
      p <- p + ggplot2::facet_wrap(~MP, ncol=ncol)
    }
  }

  if (!inc_y_label) PI_lab <- ''

  p <- p +
    ggplot2::labs(x=time_lab, y=PI_lab, color='') +
    ggplot2::scale_y_continuous(label=scales::comma, expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::theme(axis.title = ggplot2::element_text(size=size.axis.title, face='bold'),
                   axis.text = ggplot2::element_text(size=size.axis.text)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::scale_x_continuous(expand = c(0, 0))


  if (byOM | byMP) {
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(colour='#D6501C',
                                                               size=size.title, face='bold'),
                            strip.background = ggplot2::element_blank())
  }

  p

}

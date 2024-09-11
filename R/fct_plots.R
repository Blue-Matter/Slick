# slick <- readRDS('C:/users/user/downloads/Western Atlantic Skipjack.slick')


colorRampAlpha <- function(..., n, alpha) {
  colors <- grDevices::colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}


# ---- Boxplot ----

#' Plot `Boxplot`
#'
#' Plots boxplot, violin plot, or a combined box+violin plot for information stored
#' in a [Boxplot-class] object
#'
#'
#' @param slick A [Slick-class()] object
#' @param pm Numeric value indicating the performance metric to plot from the `Boxplot-class` object
#' @param type Character string specifying the plot type.
#' @param byOM Logical. Facet the plots by operating model?
#' @param include_x_labs Logical. Include MP labels on x-axis?
#' @param OM_labels Labels for the OMs if `byOM==TRUE`. Numeric sequential values used if missing
#'
#' @return A `ggplot2` object, or a list of `ggplot2` objects
#' @example inst/examples/Boxplot.R
#' @export
plotBoxplot <- function(slick, pm=1, type=c('boxplot', 'violin', 'both', 'all'),
                        byOM=FALSE, include_x_labs=TRUE,
                        OM_labels=NULL) {
  if (missing(slick))
    stop('First argument must be Slick object')
  boxplot <- slick |> Boxplot()
  values <- boxplot |> Value()
  if (all(is.na(values)))
    stop('No values in `Boxplot@Value`')
  type <- match.arg(type)
  dd <- dim(values)

  if (any(pm > dd[4]))
    return(NULL)

  nMP <- dd[3]

  Val <- values[,,,pm]

  mp_metadata <- slick |> MPs() |> Metadata()

  if (all(nchar(mp_metadata$Color)<1)) {
    mp_colors <- default_mp_colors(nMP)
  } else {
    mp_colors <-  mp_metadata$Color
  }

  if (all(nchar(mp_metadata$Color)<1)) {
    mp_names <-paste('MP ', 1:nMP)
  } else {
    mp_names <-  mp_metadata$Label
  }

  pm_names <- slick |> Boxplot() |> Code()

  df <- data.frame(Sim=1:dd[1],
                   OM=rep(1:dd[2], each=dd[1]),
                   MP=rep(mp_names, each=dd[1]*dd[2]),
                   PM=rep(pm_names[pm], each=prod(dd[1:3])),
                   value=as.vector(Val))


  df$MP <- factor(df$MP, ordered=TRUE, levels=mp_names)
  df$PM <- factor(df$PM, ordered=TRUE, levels=pm_names)

  if (!byOM) {
    box_df <- df |> dplyr::group_by(Sim, MP) |>
      dplyr::mutate(Mean=mean(value, na.rm=TRUE)) |>
      dplyr::group_by(MP) |>
      dplyr::summarize(m=median(Mean, na.rm=TRUE),
                       low1=quantile(Mean, 0.25, na.rm=TRUE),
                       upp1=quantile(Mean, 0.75, na.rm=TRUE),
                       low2=min(Mean, na.rm=TRUE),
                       upp2=max(Mean, na.rm=TRUE))
  } else {
    box_df <- df |>
      dplyr::group_by(OM, MP) |>
      dplyr::summarize(m=median(value, na.rm=TRUE),
                       low1=quantile(value, 0.25, na.rm=TRUE),
                       upp1=quantile(value, 0.75, na.rm=TRUE),
                       low2=min(value, na.rm=TRUE),
                       upp2=max(value, na.rm=TRUE))
  }

  box_df$MP <- factor(box_df$MP, ordered=TRUE, levels=mp_names)

  if (!is.null(OM_labels)) {
    OM_labels <- OM_labels
    nMP <- length(unique(box_df$MP))
    box_df$OM <- rep(OM_labels, each=nMP)
    box_df$OM <- factor(box_df$OM, levels=OM_labels, ordered = TRUE)
  }

  ymax <- max(c(1, pretty(box_df$upp2)))

  p <- ggplot2::ggplot(df |>dplyr::select(!OM), ggplot2::aes(x=MP, color=MP, fill=MP)) +
    ggplot2::scale_fill_manual(values=mp_colors) +
    ggplot2::scale_color_manual(values=mp_colors) +
    ggplot2::guides(color='none', fill='none')

  if (length(pm)>1) {
    if (byOM) {
      p <- p + ggplot2::facet_grid(OM~PM, scales='free_y') +
        ggplot2::labs(y='')
    } else {
      p <- p + ggplot2::facet_wrap(~PM) +
        ggplot2::labs(y='')
    }

  } else {
    p <- p + ggplot2::labs(x='', y='', title=pm_names[pm])
  }

  p <- p + ggplot2::expand_limits(y=c(0, ymax)) +
    ggplot2::coord_cartesian(clip = 'off') +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(legend.position='none',
                   axis.title.x = ggplot2::element_blank(),
                   axis.text=ggplot2::element_text(size=16),
                   plot.title = ggplot2::element_text(face="bold"),
                   axis.ticks.x=ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size=14, color='#D6501C'),
                   strip.background=ggplot2::element_rect(fill=NA)
    )


  if (!include_x_labs) {
    p <- p + ggplot2::theme(axis.text.x=ggplot2::element_blank())
  } else {
    p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=0.5))
  }


  p1 <- p + ggplot2::geom_linerange(data = box_df,
                                    ggplot2::aes(x=MP, ymin=low2, ymax=upp2, color=MP)) +
    ggplot2::geom_pointrange(data = box_df,
                             ggplot2::aes(x=MP, y=m, ymin=low1, ymax=upp1, color=MP, fill=MP),
                             linewidth = 2, shape = 21, inherit.aes = FALSE, size=1.5)

  if (byOM & length(pm)<2)
    p1 <- p1 + ggplot2::facet_wrap(~OM)

  p2 <- p + ggplot2::geom_violin(scale='width', ggplot2::aes(y=value))

  if (byOM & length(pm)<2)
    p2 <- p2 + ggplot2::facet_wrap(~OM)


  p3 <- p +
    ggplot2::geom_violin(scale='width', ggplot2::aes(y=value)) +
    ggplot2::geom_linerange(data = box_df, color='black',
                            ggplot2::aes(x=MP, ymin=low2, ymax=upp2)) +
    ggplot2::geom_pointrange(data = box_df,
                             ggplot2::aes(x=MP, y=m, ymin=low1, ymax=upp1),
                             linewidth = 2, shape = 21, inherit.aes = FALSE, size=1)

  if (byOM & length(pm)<2)
    p3 <- p3 + ggplot2::facet_wrap(~OM)

  if (type=='boxplot') {
    return(p1)
  }
  if (type=='violin') {
    return(p2)
  }
  if (type=='both') {
    return(p3)
  }
  list(p1, p2, p3)
}


#' @describeIn plotBoxplot  Plot multiple Performance Indicators using `cowplot::plot_grid`
#' @export
plotBoxplotGrid <- function(slick, type=c('boxplot', 'violin', 'both', 'all'), byOM=FALSE) {
  type <- match.arg(type)
  nPM <- length(slick@Boxplot@Code)

  nPM <- 9
  ncol <- min(ceiling(sqrt(nPM)),4)
  nrow <- ceiling(nPM/ncol)
  mat <- matrix(1:(nrow*ncol), nrow, ncol, byrow=TRUE)

  p_list <- list()
  rel_heights <- rep(1, nrow)
  rel_heights[nrow] <- 2

  for (i in 1:nPM) {
    p <- plotBoxplot(slick, i, type, byOM, TRUE)
    if (!i %in% mat[nrow,])
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    p_list[[i]] <- p
  }

  cowplot::plot_grid(plotlist=p_list, nrow=nrow, ncol=ncol,
                     align='v')
}

# ---- Kobe ----


# ---- Quilt ----


# ----- Spider -----


# ---- Timeseries ----


#' Plot `Timeseries`
#'
#' Plots the historical and projected values for a performance indicator.
#'
#' By default the Time Series chart plots the median, 25th and 75th quantiles as a
#' shaded ribbon, and the 10th and 90th quantiles as dashed lines.
#'
#' If the plot is by management procedure (MP), the quantiles will be included in the projections. If
#' multiple MPs are plotted together, only the median across simulations will
#' be shown in the projections.
#'
#' If `OM_ind` isn't specified, the results will be shown as the mean across
#' operating models.
#'
#' @param slick A [Slick-class()] object
#' @param PM_ind A numeric value specifying the performance indicator to plot
#' @param MP_ind A numeric value specifying the MP to plot. Defaults to NULL
#' which shows all MPs
#' @param OM_ind A numeric value specifying the OM to plot. Defaults to NULL where
#' values are calculated as mean across OMs.
#' @param OM_label A numeric or character string providing a label for `OM_ind`. Defaults to `OM_ind` if missing
#' @param includeHist Logical. Include the historical period in the projections?
#' @param col_line Color for the median line (historical)
#' @param fill_ribbon1 Fill color for the inner ribbon
#' @param col_ribbon1  Color of the line for inner ribbon
#' @param quants1 Quantiles for the inner ribbon. Numeric length 2
#' @param fill_ribbon2 Fill color for the outer ribbon
#' @param col_ribbon2 Color of the line for outer ribbon
#' @param linetype_ribbon2 Line type for outer ribbon
#' @param quants2 Quantiles for the outer ribbon. Numeric length 2.
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
#'
#' @seealso [Timeseries-methods()], [Timeseries-class()]
#' @return A `ggplot2` object
#' @export
#'
plotTimeseries <- function(slick,
                           PM_ind=1,
                           MP_ind=NULL,
                           OM_ind=NULL,
                           OM_label=NULL,
                           includeHist=TRUE,
                           col_line='darkgray',
                           fill_ribbon1='#ededed',
                           col_ribbon1='#ededed',
                           quants1=c(0.25, 0.75),
                           fill_ribbon2='white',
                           col_ribbon2='#c9c9c9',
                           linetype_ribbon2='dashed',
                           quants2=c(0.1, 0.9),
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
                           ncol=4) {

  # if (length(PM_ind)<1) return(NULL)

  timeseries <- Timeseries(slick)
  values <- Value(timeseries)
  times <- Time(timeseries)
  time_lab <- TimeLab(timeseries)
  time_now <- TimeNow(timeseries)
  target <- Target(timeseries)
  limit <- Limit(timeseries)
  metadata <- Metadata(timeseries)
  PM_lab <- metadata$Label[PM_ind]

  if (PM_ind>nrow(metadata)) {
    stop('`PM_ind` is greater than the number of performance indicators in `Timeseries(slick))`')
  }

  hist.yr.ind <- which(times<=time_now) |> max()
  hist.yrs <-  times[1:hist.yr.ind]
  proj.yr.ind <- (hist.yr.ind+1):length(times)
  proj.yrs <- times[proj.yr.ind]
  mps <- MPs(slick)
  nMP <- length(mps@Code)
  MP_lab <- slot(mps, MP_label)
  MP_colors <- Color(mps)

  nOMs <- dim(values)[2]
  if(is.null(OM_ind)) {
    oms <- 1:nOMs
  } else {
    if (length(OM_ind)>1)
      stop('`OM_ind` must be NULL or numeric length 1')

    if (OM_ind>nOMs) {
      stop('`OM_ind` is greater than number of OMs in `Timeseries(slick)`')
    }
    oms <- OM_ind
    if (is.null(OM_label))
      OM_label <- OM_ind
  }

  if (is.null(sims)) {
    sims <- 1:dim(values)[1]
  }

  p <- ggplot2::ggplot() +
    ggplot2::theme_bw()

  if (includeHist) {
    # Historical Period
    mean.hist <- apply(values[,oms,1, PM_ind,1:hist.yr.ind, drop=FALSE], c(1,4,5), mean, na.rm=TRUE) # mean over OMs
    med.hist <- apply(mean.hist, 3, median, na.rm=TRUE) # median over simulations
    quant.1.hist <- apply(mean.hist, 3, quantile, probs=quants1, na.rm=TRUE)
    quant.2.hist <- apply(mean.hist, 3, quantile, probs=quants2, na.rm=TRUE)

    # med.hist <- apply(values[sims,oms,1, PM_ind,1:hist.yr.ind, drop=FALSE], 5, median, na.rm=TRUE)
    # quant.1.hist <- apply(values[sims,oms,1,PM_ind,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=quants1, na.rm=TRUE)
    # quant.2.hist <- apply(values[sims,oms,1, PM_ind,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=quants2, na.rm=TRUE)

    Hist_df <- data.frame(x=hist.yrs,
                          Median=med.hist,
                          Lower1=quant.1.hist[1,],
                          Upper1=quant.1.hist[2,],
                          Lower2=quant.2.hist[1,],
                          Upper2=quant.2.hist[2,])


    p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower2, ymax=Upper2), data=Hist_df,
                                  color=col_ribbon2, fill=fill_ribbon2, linetype=linetype_ribbon2) +
      ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower1, ymax=Upper1), data=Hist_df,
                           fill=fill_ribbon1, col=col_ribbon1) +
      ggplot2::geom_line(ggplot2::aes(x=x, y=Median), data=Hist_df,
                         color=col_line)
  }


  # Projection
  if (any(is.na(values[sims,oms,, PM_ind,proj.yr.ind, drop=FALSE]))) {
    mean.mps <- apply(values[sims,oms,, PM_ind,proj.yr.ind, drop=FALSE], c(1,3,5), mean, na.rm=TRUE)
  } else {
    mean.mps <- apply(values[sims,oms,, PM_ind,proj.yr.ind, drop=FALSE], c(1,3,5), sum)/
      apply(values[sims,oms,, PM_ind,proj.yr.ind, drop=FALSE], c(1,3,5), length)
  }

  med.mps <- apply(mean.mps, c(2,3), median, na.rm=TRUE)
  # med.mps <- apply(values[sims,oms,, PM_ind,proj.yr.ind, drop=FALSE], c(3,5), median, na.rm=TRUE)
  meddf <- data.frame(x=rep(proj.yrs, each=nMP),
                      MP=MP_lab,
                      Median=as.vector(med.mps))
  meddf$MP <- factor(meddf$MP, ordered=TRUE, levels=MP_lab)

  if (!is.null(MP_ind)) {
    # if (length(MP_ind)>1)
      # stop('`MP_ind` must be NULL or numeric length 1')
    # if (MP_ind>nMP) {
    #   warning('`MP_ind` is greater than `nMP`. Setting to last MP')
    #   MP_ind <- nMP
    # }

    # by MP
    meddf <- meddf |> dplyr::filter(MP%in%MP_lab[MP_ind])


    quant.1.mp <- apply(mean.mps[,MP_ind,, drop=FALSE], 2:3, quantile, probs=quants1, na.rm=TRUE)
    quant.2.mp <- apply(mean.mps[,MP_ind,,drop=FALSE], 2:3, quantile, probs=quants2, na.rm=TRUE)

    # quant.1.mp <- apply(values[sims,oms,MP_ind, PM_ind,proj.yr.ind, drop=FALSE], c(3,5), quantile, probs=quants1, na.rm=TRUE)
    # quant.2.mp <- apply(values[sims,oms,MP_ind, PM_ind,proj.yr.ind, drop=FALSE], c(3,5), quantile, probs=quants2, na.rm=TRUE)

    quant_df <- data.frame(x=rep(proj.yrs, each=length(MP_ind)),
                           MP=MP_lab[MP_ind],
                           Lower1=as.vector(quant.1.mp[1,,]),
                           Upper1=as.vector(quant.1.mp[2,,]),
                           Lower2=as.vector(quant.2.mp[1,,]),
                           Upper2=as.vector(quant.2.mp[2,,]))
    quant_df$MP <- factor(quant_df$MP, ordered=TRUE, levels=MP_lab[MP_ind])

    p <- p +
      ggplot2::facet_wrap(~MP, ncol=ncol) +
      ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower2, ymax=Upper2), data=quant_df,
                           color=col_ribbon2, fill=fill_ribbon2, linetype=linetype_ribbon2) +
      ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower1, ymax=Upper1), data=quant_df,
                           fill=fill_ribbon1, col=col_ribbon1) +
      ggplot2::geom_line(ggplot2::aes(x=x, y=Median, color=MP), data=meddf) +
      ggplot2::scale_color_manual(values=MP_colors[MP_ind]) +
      ggplot2::guides(color='none')

    if (length(MP_ind)<2) {
      p <- p + ggplot2::labs(title=MP_lab[MP_ind]) +
        ggplot2::theme(plot.title =ggplot2::element_text(colour=MP_colors[MP_ind],
                                                         face='bold', hjust = 0.5,
                                                         size=size.title))
    } else {
      p <- p + ggplot2::theme(strip.text = ggplot2::element_text(size=size.title))
    }


  } else {
    meddf_last <- meddf |> dplyr::filter(x==max(x, na.rm=TRUE))
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=x, y=Median, color=MP), data=meddf) +
      ggplot2::scale_color_manual(values=MP_colors) +
      ggplot2::guides(color='none')

    if (!is.null(size.mp.label)) {
      p <- p + ggrepel::geom_text_repel(data=meddf_last,
                                        ggplot2::aes(x=x, y=Median,
                                                     label=MP),
                                        color=MP_colors,
                                        size=size.mp.label,
                                        min.segment.length=2)
    }

  }


  if(!is.null(OM_ind)) {
    # by OM
    p <- p +  ggplot2::labs(title=paste('OM', OM_label)) +
      ggplot2::theme(plot.title =ggplot2::element_text(colour='#D6501C',
                                                       face='bold',
                                                       hjust = 0.5,
                                                       size=size.title)) +
      ggplot2::guides(color='none')
  }


  # add target and limit lines if they exist
  if (!is.null(target)) {
    targ <- target[PM_ind]
    if (!is.na(targ)) {
      x_loc <- times[1]
      if (!includeHist) {
        x_loc <- time_now+1
      }

      p <- p + ggplot2::geom_hline(yintercept = targ, color=targ_color,
                                     alpha=0.5) +
        ggrepel::geom_text_repel(x=x_loc, y=targ,
                                 label=targ_name, color=targ_color)
    }
  }
  if (!is.null(limit)) {
    lim <- limit[PM_ind]
    if (!is.na(targ)) {
      x_loc <- times[1]
      if (!includeHist) {
        x_loc <- time_now+1
      }

      p <- p + ggplot2::geom_hline(yintercept = lim, color=lim_color,
                                   alpha=0.5) +
        ggrepel::geom_text_repel(x=x_loc, y=lim,
                                 label=lim_name, color=lim_color)
    }
  }

  if (!inc_y_label) PM_lab <- ''

  p <- p +
    ggplot2::labs(x=time_lab, y=PM_lab, color='') +
    ggplot2::scale_y_continuous(label=scales::comma) +
    ggplot2::theme(axis.title = ggplot2::element_text(size=size.axis.title, face='bold'),
                   axis.text = ggplot2::element_text(size=size.axis.text)) +
    ggplot2::expand_limits(y=0) +
    ggplot2::scale_x_continuous(expand = c(0, 0))

  p

}

# ---- Tradeoff ----


quilt_kable <- function(Values, metadata_pm, cols) {

  if (!requireNamespace('flextable', quietly = TRUE)) {
    stop('package `flextable` required for this function')
  }

  table <- flextable::flextable(data.frame(Values) |>
                                  tibble::rownames_to_column('MP'))


  for (i in 1:ncol(Values)) {
    minVal <- metadata_pm$MinValue[i]
    maxVal <- max(metadata_pm$MaxValue[i], Values[,i], na.rm=TRUE)

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

  quilt <- Quilt(slick)

  # quilt <<-  Quilt(slick)
  # MP_labels <<- MP_labels
  # kable <<- kable
  # lang=NULL


  Values <- Value(quilt) |>
    apply(2:3, mean, na.rm=TRUE) |>
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
  metadata_pm$MinValue <- MinValue(quilt)
  metadata_pm$MaxValue <- ifelse(length(MaxValue(quilt)>0),MaxValue(quilt),1)
  colnames(Values) <- metadata_pm$Code
  cols <- Color(quilt)
  if (kable) {
    return(quilt_kable(Values, metadata_pm, cols))
  }
  quilt_DT(Values, metadata_pm, cols)
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
  sq<-2*3.141593*(0:n)/n
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
    text_color <- rep('#FF4500', length(mean_over_MPs))
    # rgbvals <- grDevices::col2rgb(MP_metadata$Color)
    # luma <- apply(rgbvals, 2, get_luma)
    # text_color[luma<40] <- 'lightgrey'
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

  # mean across OMs
  Values <- slick |> Spider() |>
    Value() |> apply(2:3, mean, na.rm=TRUE)

  if(relative_scale) {
    # make all PMs relative to maximum and minimum values
    Values <- apply(Values, 2, normalize) * 100
    Values[!is.finite(Values)] <- 100
  }

  # average MP value across PMs
  if(include_avg) {
    mp.avg <- apply(Values, 1, mean, na.rm=TRUE)
    text_color <- rep('#FF4500', length(mp.avg))
    # rgbvals <- grDevices::col2rgb(cols)
    # luma <- apply(rgbvals, 2, get_luma)
    # text_color[luma<40] <- 'lightgrey'
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

  # mean across OMs
  metadata_MP <- slick |> MPs() |> Metadata()
  nMPs <- nrow(metadata_MP)

  spider <- slick |> Spider()
  Values <- spider |>
    Value() |> apply(2:3, mean, na.rm=TRUE)

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

    # Calc mean PM over OMs
    pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), mean, na.rm=TRUE)
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




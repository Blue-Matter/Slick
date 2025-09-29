get_MP_info <- function(slick, MP_label='Code', nMP=NULL) {
  mps <- MPs(slick)
  mps <- MPs(slick)
  if (is_empty(mps)) {
    if (is.null(nMP))
      cli::cli_abort('`nMP` must be specified in `MPs` is empty')

    cli::cli_alert_info('Note: `MPs` is empty. Using default MP names and colors')
    MP_lab <- paste('MP', 1:nMP)
    MP_colors <- default_mp_colors(nMP)
  } else {
    MP_lab <- slot(mps, MP_label)
    MP_colors <- Color(mps)
  }
  MP_lab <- factor(MP_lab, levels=MP_lab, ordered = TRUE)
  list(MP_lab=MP_lab, MP_colors=MP_colors)
}

get_OM_labels <- function(slick, nOM=NULL) {
  if (is_empty(slick@OMs)) {
    if (is.null(nOM))
      cli::cli_abort('`nOM` must be specified in `OMs` is empty')
    OM_labels <- paste('OM', 1:nOM)
  } else {
    OM_labels <- rownames(slick@OMs@Design)
  }
  OM_labels <- factor(OM_labels, levels=OM_labels, ordered = TRUE)
  OM_labels
}

#' Plot `Boxplot`
#'
#' Plots boxplot, violin plot, or a combined box+violin plot for information stored
#' in a [Boxplot-class] object
#'
#'
#' @param slick A [Slick-class()] object
#' @param PI Numeric value indicating the Performance Indicator(s) to plot from the `Boxplot-class` object.
#' If NULL, it will facet_wrap all PIs
#' @param type Character string specifying the plot type.
#' @param byOM Logical. Facet the plots by operating model? PI must be a single value
#' @param ncol Numeric. Number of columns
#' @param MP_label Label to use for the MPs. Either `Code` or `Label`.
#' `Description` works as well, but you probably don't want to do that.
#' @param PI_label Label to use for the PIs. Either `Code` or `Label`.
#' `Description` works as well, but you probably don't want to do that.
#'
#' @return A `ggplot2` object, or a list of `ggplot2` objects
#' @example inst/examples/Boxplot.R
#' @export
plotBoxplot <- function(slick, PI=NULL, type=c('boxplot', 'violin', 'both', 'all'),
                        byOM=FALSE, ncol=4, MP_label='Code', PI_label='Code') {

  type <- match.arg(type)

  slick <- Update(slick)

  Mean <- OM <- NULL # CRAN checks

  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  boxplot <- Boxplot(slick)
  chk <- Check(boxplot)
  if (chk@empty)
    cli::cli_abort('`Boxplot` in this `Slick` object is empty. Use  {.code Check(slick)}')

  if (!chk@complete)
    cli::cli_abort('`Boxplot` in this `Slick` object is incomplete. Use  {.code Check(slick)}')


  values <- boxplot |> Value()
  if (all(is.na(values)))
    cli::cli_abort('No values in `Boxplot@Value`')

  dd <- dim(values)
  if (any(PI > dd[4]))
    return(NULL)


  nsim <- dd[1]
  nOM <- dd[2]
  nMP <- dd[3]
  nPI <- dd[4]

  MP_info <- get_MP_info(slick, MP_label, nMP)
  MP_lab <- MP_info$MP_lab
  MP_colors <- MP_info$MP_colors

  OM_labels <- get_OM_labels(slick, nOM)
  PI_names <- slot(boxplot, PI_label)

  Sim <- OM <- MP <- value <- NULL # CRAN checks

  df <- data.frame(Sim=1:nsim,
                   OM=rep(OM_labels, each=nsim),
                   MP=rep(MP_lab, each=nsim * nOM),
                   PI=rep(PI_names, each=nsim * nOM * nMP),
                   value=as.vector(values))
  df$MP <- factor(df$MP, ordered=TRUE, levels=MP_lab)
  df$PI <- factor(df$PI, ordered=TRUE, levels=PI_names)
  df$OM <- factor(df$OM, levels=OM_labels, ordered = TRUE)

  if (byOM) {
    if (is.null(PI)) {
      PI <- 1
    } else {
      PI <- PI[1]
    }
  }

  if (!is.null(PI)) {
    pi <- PI
    df <- df |> dplyr::filter(PI==PI_names[pi])
  }


  if (!byOM) {
    box_df <- df |> dplyr::group_by(Sim, MP, PI) |>
      dplyr::mutate(Mean=mean(value, na.rm=TRUE)) |>
      dplyr::group_by(MP, PI) |>
      dplyr::summarize(m=median(Mean, na.rm=TRUE),
                       low1=quantile(Mean, 0.25, na.rm=TRUE),
                       upp1=quantile(Mean, 0.75, na.rm=TRUE),
                       low2=min(Mean, na.rm=TRUE),
                       upp2=max(Mean, na.rm=TRUE))
    box_df$MP <- factor(box_df$MP, ordered=TRUE, levels=MP_lab)
    box_df$PI <- factor(box_df$PI, ordered=TRUE, levels=PI_names)
  } else {
    box_df <- df |>
      dplyr::group_by(OM, MP, PI) |>
      dplyr::summarize(m=median(value, na.rm=TRUE),
                       low1=quantile(value, 0.25, na.rm=TRUE),
                       upp1=quantile(value, 0.75, na.rm=TRUE),
                       low2=min(value, na.rm=TRUE),
                       upp2=max(value, na.rm=TRUE))

    box_df$MP <- factor(box_df$MP, ordered=TRUE, levels=MP_lab)
    box_df$PI <- factor(box_df$PI, ordered=TRUE, levels=PI_names)


    nMP <- length(unique(box_df$MP))
    box_df$OM <- rep(OM_labels, each=nMP)
    box_df$OM <- factor(box_df$OM, levels=OM_labels, ordered = TRUE)
  }


  p <- ggplot2::ggplot(df, ggplot2::aes(x=MP, color=MP, fill=MP)) +
    ggplot2::labs(y='') +
    ggplot2::scale_fill_manual(values=MP_colors) +
    ggplot2::scale_color_manual(values=MP_colors) +
    ggplot2::guides(color='none', fill='none') +
    ggplot2::expand_limits(y=0) +
    ggplot2::coord_cartesian(clip = 'off') +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(legend.position='none',
                   axis.title.x = ggplot2::element_blank(),
                   axis.text=ggplot2::element_text(size=16),
                   plot.title = ggplot2::element_text(face="bold", size=18),
                   axis.ticks.x=ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size=18, color='#D6501C',
                                                      margin = ggplot2::margin(0.5,0,0.5,0, "cm")),
                   strip.background=ggplot2::element_rect(fill=NA),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)

    )

  if (!byOM) {
    p <- p + ggplot2::facet_wrap(~PI, scales='free_y', ncol=ncol)
  } else {
    p <- p + ggplot2::facet_wrap(~OM,  ncol=ncol) +
      ggplot2::labs(title=PI_names[PI])
  }


  p1 <- p + ggplot2::geom_linerange(data = box_df,
                                    ggplot2::aes(x=MP, ymin=low2, ymax=upp2, color=MP)) +
    ggplot2::geom_pointrange(data = box_df,
                             ggplot2::aes(x=MP, y=m, ymin=low1, ymax=upp1, color=MP, fill=MP),
                             linewidth = 2, shape = 21, inherit.aes = FALSE, size=1.5)



  p2 <- p + ggplot2::geom_violin(scale='width', ggplot2::aes(y=value))

  p3 <- p +
    ggplot2::geom_violin(scale='width', ggplot2::aes(y=value)) +
    ggplot2::geom_linerange(data = box_df, color='black',
                            ggplot2::aes(x=MP, ymin=low2, ymax=upp2)) +
    ggplot2::geom_pointrange(data = box_df,
                             ggplot2::aes(x=MP, y=m, ymin=low1, ymax=upp1),
                             linewidth = 2, shape = 21, inherit.aes = FALSE, size=1)

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

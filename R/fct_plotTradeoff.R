#' Plot `Tradeoff`
#'
#'
#' @param slick A [Slick-class()] object
#' @param xPI Numeric value indicating the PI to plot on the x-axis. Multiple values are accepted. Recycled if `xPI<yPI`
#' @param yPI Numeric value indicating the PI to plot on the y-axis. Multiple values are accepted. Recycled if `yPI<xPI`
#' @param OMs Integers representing the OMs to include in the plot. Defaults to all.
#' @param MP_label Label to use for the MPs. Either `Code` or `Label`.
#' `Description` works as well, but you probably don't want to do that.
#' @param lab_size Size of the MP labels
#' @param point_size Size of the points
#' @param size.axis.title Size of axis title
#' @param size.axis.text Size of axis text
#' @example inst/examples/Tradeoff.R
#' @return A `ggplot2` object
#' @export
plotTradeoff <- function(slick,
                         xPI=NULL,
                         yPI=NULL,
                         OMs=NA,
                         MP_label='Code',
                         lab_size=6,
                         point_size=2,
                         size.axis.title=14,
                         size.axis.text=12) {

  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  if(!requireNamespace("cowplot", quietly = TRUE)) {
    cli::cli_abort("The `cowplot` package is required for this function: `install.packages('cowplot')", call. = FALSE)
  }

  slick <- Update(slick)

  if (!all(is.na(OMs))) {
    slick <- FilterSlick(slick, OMs=OMs, plot='Tradeoff')
  }

  tradeoff <- Tradeoff(slick)
  chk <- Check(tradeoff)
  if (chk@empty)
    cli::cli_abort('`Tradeoff` in this `Slick` object is empty. Use  {.code Check(slick)}')

  if (!chk@complete)
    cli::cli_abort('`Tradeoff` in this `Slick` object is incomplete. Use  {.code Check(slick)}')


  # mean over OMs
  Values <- Value(tradeoff) |>  apply(2:3, mean)

  if (all(is.na(Values))) {
    cli::cli_alert_danger('`Value` is all NA')
    return(NULL)
  }

  PI_label <- tradeoff@Label
  nPI <- length(PI_label)


  nMP <- dim(Values)[1]
  MP_info <- get_MP_info(slick, MP_label, nMP)
  mp_labels <- MP_info$MP_lab
  mp_colors <- MP_info$MP_colors

  if (is.null(xPI))
    xPI <- 1
  if (is.null(yPI))
    yPI <- 2

  if (any((xPI>nPI))) {
    ind <- which(xPI>nPI)
    cli::cli_alert_danger(paste0('Some `xPI`> greater than `nPI` (', nPI, ')\n Setting to 1'))
    xPI[ind] <- 1
  }
  if (any(yPI>nPI)) {
    ind <- which(yPI>nPI)
    cli::cli_alert_danger(paste0('`Some yPI`> greater than `nPI` (', nPI, ')\n Setting to 2'))
    yPI[ind] <- 2
  }

  if (length(xPI)>1 | length(yPI)>1) {
    nX <- length(xPI)
    nY <- length(yPI)
    if (nX < nY)
      xPI <- rep(xPI, nY)[1:nY]
    if (nY < nX)
      yPI <- rep(yPI, nX)[1:nX]
  }

  plot_list <- list()

  for (p in seq_along(xPI)) {
    xlab <- PI_label[xPI[p]]
    ylab <- PI_label[yPI[p]]
    x_value <- Values[,xPI[p]]
    y_value <- Values[, yPI[p]]

    if (length(x_value)<1) return(NULL)

    df <- data.frame(x=x_value, y=y_value,
                     MP=mp_labels,
                     Color=mp_colors)

    xmax <- x_value |> pretty() |> max()
    xmax <- max(c(xmax, 1))
    ymax <- y_value |> pretty() |> max()
    ymax <- max(c(ymax, 1))

    df$MP <- factor(df$MP, ordered = TRUE, levels=unique(df$MP))

    plot_list[[p]] <- ggplot2::ggplot(df, ggplot2::aes(x=x, y=y, color=MP)) +
      ggplot2::geom_point(size=point_size) +
      ggplot2::theme_bw() +
      ggplot2::expand_limits(x=c(0, xmax), y=c(0, ymax)) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult=c(0,0.02))) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::coord_cartesian(clip='off') +
      ggrepel::geom_text_repel(ggplot2::aes(label=MP), size=lab_size,
                               max.overlaps=15) +
      ggplot2::scale_color_manual(values=df$Color) +
      ggplot2::guides(color='none') +
      ggplot2::labs(x=xlab, y=ylab) +
      ggplot2::theme(axis.title = ggplot2::element_text(size=size.axis.title),
                     axis.text = ggplot2::element_text(size=size.axis.text))

  }

  if (length(plot_list)==1)
    return(plot_list[[1]])
  cowplot::plot_grid(plotlist=plot_list)
}



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
                      inc_line=TRUE) {

  kobe <- Kobe(slick)
  metadata <- Metadata(kobe)
  times <- Time(kobe)

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
    ggplot2::geom_polygon(data=bgDF, ggplot2::aes(x=x, y=y, fill=lab), alpha=0.4) +
    ggplot2::scale_fill_manual(values=bgCols) +
    ggplot2::guides(fill="none", label="none") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x=metadata$Label[xvar],
                  y=metadata$Label[yvar])


  df <- data.frame(x=as.vector(apply(values[,,,xvar,, drop=FALSE], c(3,5), median, na.rm=TRUE)),
                   y=as.vector(apply(values[,,,yvar,, drop=FALSE], c(3,5), median, na.rm=TRUE)),
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

  if (inc_line) {
    p <- p +
      ggplot2::geom_line(data=df, ggplot2::aes(x=x, y=y, color=MP)) +
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

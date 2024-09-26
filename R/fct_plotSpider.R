#' Plot `Spider`
#'
#' A Spider or Radar plot
#'
#'
#' @param slick A [Slick-class()] object
#'
#' @seealso [Slick-methods()], [Slick-class()]
#' @return A `ggplot2` object
#' @export
#'

plotSpider <- function(slick) {

  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  spider <- Spider(slick)
  chk <- Check(spider)
  if (chk@empty)
    cli::cli_abort('`Spider` in this `Slick` object is empty. Use  {.code Check(slick)}')

  if (!chk@complete)
    cli::cli_abort('`Spider` in this `Slick` object is incomplete. Use  {.code Check(slick)}')


  Values <- spider |>
    Value() |> apply(2:3, mean, na.rm=TRUE)

  nPI <- ncol(Values)
  nMP <- nrow(Values)

  mp_colors <- slick |> MPs() |> Color()


  #######################

  library(fmsb)

  df <- rbind(rep(1, nPI),
        rep(0, nPI),
        Values) |> data.frame()
  colnames(df) <-  spider@Code



  fmsb::radarchart(df, pcol=mp_colors)


  #######################

  # devtools::install_github("ricardo-bion/ggradar",
  #                          dependencies = TRUE)

  library(ggradar)

  df <- data.frame(Values)
  colnames(df) <-  toupper(letters[seq_along(spider@Code)])
  df <- df |> dplyr::mutate(MP=slick@MPs@Code) |>
    dplyr::select(MP, toupper(letters[seq_along(spider@Code)]))

  ggradar(df, group.point.size=0,
          values.radar='',
          plot.legend=FALSE) +
    ggplot2::scale_color_manual(values=mp_colors)

          axis.label.size = 12) +
    ggplot2::theme(text=ggplot2::element_text(color='blue'))


    ggplot2::theme(axis.title=ggplot2::element_text(color='blue'))


  df1 <- df |> dplyr::filter(MP=='AvC')
  df2 <- df |> dplyr::filter(MP=='Itarget1')
  df3 <- df |> dplyr::filter(MP=='DD')

  library(patchwork)
  p1 <- ggradar(df1) + ggplot2::guides(color='none') +
    ggplot2::scale_color_manual(values=mp_colors[1])
  p2 <- ggradar(df2) + ggplot2::guides(color='none') +
    ggplot2::scale_color_manual(values=mp_colors[2])
  p3 <- ggradar(df3) + ggplot2::guides(color='none') +
    ggplot2::scale_color_manual(values=mp_colors[3])

  textDF <- data.frame(x=0, y=0, text='77.5')


  p1 <- ggradar(df1, fill=TRUE,
                group.point.size=0,
                values.radar='') + ggplot2::guides(color='none', fill='none') +
    ggplot2::scale_color_manual(values=mp_colors[1]) +
    ggplot2::scale_fill_manual(values=mp_colors[1]) +
    ggplot2::geom_text(data=textDF, ggplot2::aes(x=x, y=y, label=text))


  p1

  p1 + p2 + p3

}

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
plotSpider <- function(slick,
                       byOM=FALSE,
                       byMP=FALSE,
                       incMean=FALSE,
                       incMax=FALSE,
                       size.pi.label=6,
                       size.mean=4,
                       size.mp.label=16,
                       size.max.value=4,
                       size.om.title=5,
                       col.om.title="#D6501C",
                       relScale=FALSE,
                       fill=byMP|byOM,
                       MP_label='Code',
                       ncol=4) {

  if (!methods::is(slick, 'Slick'))
    cli::cli_abort('`slick` must be an object of class `Slick`')

  mps <- MPs(slick)
  mp_colors <- Color(mps)
  mp_labels <- slot(mps, MP_label)

  spider <- Spider(slick)
  chk <- Check(spider)
  if (chk@empty)
    cli::cli_abort('`Spider` in this `Slick` object is empty. Use  {.code Check(slick)}')

  if (!chk@complete)
    cli::cli_abort('`Spider` in this `Slick` object is incomplete. Use  {.code Check(slick)}')

  if (byOM) {
    Values <-  Value(spider)
    dd <- dim(Values)
    nOM <- dd[1]
    nMP <- dd[2]
    nPI <- dd[3]

    om_names <- rownames(slick@OMs@Design)
    om_list <- list()
    for (om in 1:nOM) {
      df <- data.frame(Values[om,,])
      meanMP <- apply(df[,-1], 1, mean, na.rm=TRUE)
      ord <- order(meanMP, decreasing =TRUE)
      colnames(df) <-  toupper(letters[seq_along(spider@Code)])
      df <- df |> dplyr::mutate(MP=slick@MPs@Code) |>
        dplyr::select(MP, toupper(letters[seq_along(spider@Code)]))

      plot_list <- spider_by_MP(df, mp_labels='', mp_colors,
                                size.pi.label=0,
                                size.mp.label, size.mean,
                                size.max.value,
                                fill=fill,
                                incMax)

      plot_list <- plot_list[ord]
      plot_list[[1]] <- plot_list[[1]] +
        ggplot2::labs(title=om_names[om]) +
        ggplot2::theme(title = ggplot2::element_text(size=size.om.title,
                                                     color=col.om.title))

      om_list[[om]] <- patchwork::wrap_plots(plot_list, ncol=1)

    }

    p <- patchwork::wrap_plots(om_list, ncol=min(5,nOM))

  } else {
    Values <- Value(spider) |> apply(2:3, mean, na.rm=TRUE)
    nMP <- nrow(Values)
    nPI <- ncol(Values)

    df <- data.frame(Values)
    colnames(df) <-  toupper(letters[seq_along(spider@Code)])
    df <- df |> dplyr::mutate(MP=slick@MPs@Code) |>
      dplyr::select(MP, toupper(letters[seq_along(spider@Code)]))

    if (byMP) {
      plot_list <- spider_by_MP(df, mp_labels, mp_colors,
                                size.pi.label=0,
                                size.mp.label, size.mean,
                               size.max.value,
                               fill,
                               incMax)
      p <- patchwork::wrap_plots(plot_list, ncol=ncol)


    } else {

      p <- suppressWarnings(myggradar(df, group.point.size=0,
                     draw.points = FALSE,
                     values.radar='',
                     group.colours=mp_colors,
                     fill=fill,
                     plot.legend=FALSE))



    }
  }
  suppressWarnings(p)
}


spider_by_MP <- function(df, mp_labels,
                         mp_colors,
                         size.pi.label,
                         size.mp.label,
                         size.mean,
                         size.max.value,
                         fill,
                         incMax) {
  meanMP <- apply(df[,-1], 1, mean, na.rm=TRUE)
  plot_list <- list()

  for (i in seq_along(1:nrow(df))) {
    textDF <- data.frame(x=0, y=0, text=signif(meanMP[i], 2))
    textDF$text <- gsub("^([^\\.]+)$", "\\1\\.00", gsub("\\.(\\d)$", "\\.\\10",
                                                        textDF$text))

    mp_label <- mp_labels[i]
    if (is.na(mp_label)) mp_label <- ''
    plot_list[[i]] <- myggradar(df[i,],
                                group.point.size=0,
                                draw.points = FALSE,
                                axis.label.size = size.pi.label,
                                values.radar='',
                                group.colours=mp_colors[i],
                                fill=fill,
                                plot.legend=FALSE,
                                plot.title = mp_label,
                                draw.max.point=incMax,
                                size.max.value=size.max.value) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5,
                                                        size=size.mp.label))






    if (incMean)
      plot_list[[i]] <- plot_list[[i]] +
      ggplot2::geom_label(data=textDF,
                          ggplot2::aes(x=x, y=y, label=text),
                          size=size.mean)


  }
  plot_list
}




# This code is a slightly modified version of ``ggradar` from \url{https://github.com/ricardo-bion/ggradar}.

myggradar <- function(plot.data,
                    base.size = 15,
                    font.radar = "sans",
                    values.radar = c("0%", "50%", "100%"),
                    axis.labels = colnames(plot.data)[-1],
                    grid.min = 0,
                    grid.mid = 0.5,
                    grid.max = 1,
                    centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
                    plot.extent.x.sf = 1,
                    plot.extent.y.sf = 1.2,
                    x.centre.range = 0.02 * (grid.max - centre.y),
                    label.centre.y = FALSE,
                    grid.line.width = 0.5,
                    gridline.min.linetype = "longdash",
                    gridline.mid.linetype = "longdash",
                    gridline.max.linetype = "longdash",
                    gridline.min.colour = "grey",
                    gridline.mid.colour = "#007A87",
                    gridline.max.colour = "grey",
                    grid.label.size = 6,
                    gridline.label.offset = -0.1 * (grid.max - centre.y),
                    label.gridline.min = TRUE,
                    label.gridline.mid = TRUE,
                    label.gridline.max = TRUE,
                    axis.label.offset = 1.15,
                    axis.label.size = 5,
                    axis.line.colour = "grey",
                    group.line.width = 1.5,
                    group.point.size = 6,
                    group.colours = NULL,
                    background.circle.colour = "#D7D6D1",
                    background.circle.transparency = 0.2,
                    plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE,
                    legend.title = "",
                    plot.title = "",
                    legend.text.size = 14,
                    legend.position = "left",
                    fill = FALSE,
                    fill.alpha = 0.5,
                    draw.points = TRUE,
                    point.alpha = 1,
                    line.alpha = 1,
                    draw.max.point=FALSE,
                    size.max.value
) {
  plot.data <- as.data.frame(plot.data)

  plot.data <- aggregate(
    x = plot.data[, -1],
    by = list(plot.data[, 1]),
    FUN = "mean")

  if (!is.factor(plot.data[, 1])) {
    plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
  }

  var.names <- colnames(plot.data)[-1]

  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf

  # Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data) - 1) {
    stop("'axis.labels' contains the wrong number of axis labels", call. = FALSE)
  }
  if (min(plot.data[, -1]) < centre.y) {
    stop("plot.data' contains value(s) < centre.y", call. = FALSE)
  }

  if (max(plot.data[, -1]) > grid.max) {
    plot.data[, -1] <- (plot.data[, -1]/max(plot.data[, -1]))*grid.max
    warning("'plot.data' contains value(s) > grid.max, data scaled to grid.max", call. = FALSE)
  }

  plot.data.offset <- plot.data
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)

  group <- NULL
  group$path <- CalculateGroupPath(plot.data.offset)

  axis <- NULL
  axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))

  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )

  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })

  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0, 0), grid.min + abs(centre.y), npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0, 0), grid.mid + abs(centre.y), npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0, 0), grid.max + abs(centre.y), npoints = 360)

  gridline$min$label <- data.frame(
    x = gridline.label.offset, y = grid.min + abs(centre.y),
    text = as.character(grid.min)
  )
  gridline$max$label <- data.frame(
    x = gridline.label.offset, y = grid.max + abs(centre.y),
    text = as.character(grid.max)
  )
  gridline$mid$label <- data.frame(
    x = gridline.label.offset, y = grid.mid + abs(centre.y),
    text = as.character(grid.mid)
  )

  theme_clear <- ggplot2::theme_bw(base_size = base.size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(linetype = "blank")
    )

  if (plot.legend == FALSE) legend.position <- "none"

  base <- ggplot2::ggplot(axis$label) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::coord_equal() +
    ggplot2::geom_text(
      data = subset(axis$label, axis$label$x < (-x.centre.range)),
      ggplot2::aes(x = x, y = y, label = text), size = axis.label.size, hjust = 1, family = font.radar
    ) +
    ggplot2::scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    ggplot2::scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y))

  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base + ggplot2::geom_path(
    data = gridline$min$path, ggplot2::aes(x = x, y = y),
    lty = gridline.min.linetype, colour = gridline.min.colour, linewidth = grid.line.width
  )
  base <- base + ggplot2::geom_path(
    data = gridline$mid$path, ggplot2::aes(x = x, y = y),
    lty = gridline.mid.linetype, colour = gridline.mid.colour, linewidth = grid.line.width
  )
  base <- base + ggplot2::geom_path(
    data = gridline$max$path, ggplot2::aes(x = x, y = y),
    lty = gridline.max.linetype, colour = gridline.max.colour, linewidth = grid.line.width
  )

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + ggplot2::geom_text(
    data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
    ggplot2::aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0.5, family = font.radar
  )
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + ggplot2::geom_text(
    data = subset(axis$label, axis$label$x > x.centre.range),
    ggplot2::aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0, family = font.radar
  )
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + ggplot2::geom_polygon(
    data = gridline$max$path, ggplot2::aes(x, y),
    fill = background.circle.colour,
    alpha = background.circle.transparency
  )

  # + radial axes
  base <- base + ggplot2::geom_path(
    data = axis$path, ggplot2::aes(x = x, y = y, group = axis.no),
    colour = axis.line.colour
  )

  theGroupName <- names(group$path[1])

  if (length(line.alpha) == 1) {
    base <- base + ggplot2:: geom_path(data = group$path,
                                      ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                                                   group = .data[[theGroupName]],
                                                   colour = .data[[theGroupName]]),
                                      linewidth = group.line.width, alpha = line.alpha)
  } else {
    # Assuming line.alpha is a vector with the same length as the number of groups
    # This will apply different alpha values to each line
    base <- base + ggplot2::geom_path(data = group$path,
                                      ggplot2::aes(x = .data[["x"]],
                                                   y = .data[["y"]],
                                                   group = .data[[theGroupName]],
                                                   colour = .data[[theGroupName]]),
                                      linewidth = group.line.width) +
      ggplot2::scale_alpha_manual(values = line.alpha)
  }

  # ... + group points (cluster data)
  # Modify point drawing logic based on draw.points
  if (draw.points) {
    # Check if point.alpha is a vector or single value
    if (length(point.alpha) == 1) {
      base <- base + ggplot2::geom_point(data = group$path,
                                         ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                                                      group = .data[[theGroupName]],
                                                      colour = .data[[theGroupName]]),
                                         size = group.point.size, alpha = point.alpha)
    } else {
      # Assuming point.alpha is a vector with the same length as the number of groups
      # This will apply different alpha values to each group
      base <- base + ggplot2::geom_point(data = group$path,
                                ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = .data[[theGroupName]], colour = .data[[theGroupName]]), size = group.point.size) +
        ggplot2::scale_alpha_manual(values = point.alpha)
    }
  }

  if (draw.max.point) {
    maxV <- max(plot.data[,-1])
    maxInd <- which( plot.data[,-1]==maxV)
    maxValDF <- group$path[maxInd,]
    base <- base + ggplot2::geom_point(data =maxValDF,
                                       ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                                                    group = .data[[theGroupName]],
                                                    colour = .data[[theGroupName]]),
                                       size = size.max.value, alpha = point.alpha)


  }

  # ... + group (cluster) fills
  if (fill == TRUE) {
    base <- base + ggplot2::geom_polygon(data = group$path, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], group = .data[[theGroupName]], fill = .data[[theGroupName]]), alpha = fill.alpha)
  }


  # ... + amend Legend title
  if (plot.legend == TRUE) base <- base + ggplot2::labs(colour = legend.title, size = legend.text.size)

  # ... + grid-line labels (max; mid; min)
  if (label.gridline.min == TRUE) {
    base <- base + ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = values.radar[1]), data = gridline$min$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.mid == TRUE) {
    base <- base + ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = values.radar[2]), data = gridline$mid$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  if (label.gridline.max == TRUE) {
    base <- base + ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = values.radar[3]), data = gridline$max$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text), data = centre.y.label, size = grid.label.size, hjust = 0.5, family = font.radar)
  }

  if (!is.null(group.colours)) {
    colour_values <- rep(group.colours, length(unique(plot.data[, 1])) / length(group.colours))
  } else {
    colour_values <- generate_color_values(length(unique(plot.data[, 1])))
  }

  base <- base +
    ggplot2::theme(
      legend.key.width = ggplot2::unit(3, "line"),
      text = ggplot2::element_text(
        size = 20,
        family = font.radar
      )
    ) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = legend.text.size),
              legend.position = legend.position) +
    ggplot2::theme(legend.key.height = ggplot2::unit(2, "line")) +
    ggplot2::scale_colour_manual(values = colour_values) +
    ggplot2::theme(text = ggplot2::element_text(family = font.radar)) +
    ggplot2::theme(legend.title = ggplot2::element_blank())


  if (isTRUE(fill)) {
    base <- base +
      ggplot2::scale_fill_manual(values = colour_values, guide = "none")
  }

  if (legend.title != "") {
    base <- base + theme(legend.title = ggplot2::element_text())
  }

  if (plot.title != "") {
    base <- base + ggplot2::ggtitle(plot.title)
  }

  return(base)
}

# borrowed from: https://github.com/ricardo-bion/ggradar
CalculateGroupPath <- function(df) {
  # Drop dead levels. This might happen if the data is filtered on the way
  # into ggradar.
  path <- forcats::fct_drop(df[, 1])
  # set the name of the variable that will be used for grouping
  theGroupName <- colnames(df)[1]

  ## find increment
  nPathPoints <- ncol(df) - 1
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / nPathPoints)
  ## create graph data frame
  nDataPoints <- ncol(df) * length(levels(path))
  graphData <- data.frame(
    seg = rep("",nDataPoints),
    x = rep(0, nDataPoints),
    y = rep(0, nDataPoints))
  colnames(graphData)[1] <- theGroupName

  rowNum <- 1
  for (i in 1:length(levels(path))) {
    pathData <- subset(df, df[, 1] == levels(path)[i])
    for (j in c(2:ncol(df))) {
      graphData[rowNum,theGroupName] <- levels(path)[i]
      graphData$x[rowNum] <- pathData[, j] * sin(angles[j - 1])
      graphData$y[rowNum] <- pathData[, j] * cos(angles[j - 1])
      rowNum <- rowNum + 1
    }
    ## complete the path by repeating first pair of coords in the path
    graphData[rowNum,theGroupName] <- levels(path)[i]
    graphData$x[rowNum] <- pathData[, 2] * sin(angles[1])
    graphData$y[rowNum] <- pathData[, 2] * cos(angles[1])
    rowNum <- rowNum + 1
  }
  # Make sure that name of first column matches that of input data (in case !="group")
  graphData[,1] <- factor(graphData[,1], levels=levels(path) ) # keep group order
  graphData # data frame returned by function
}


CalculateAxisPath <- function(var.names, min, max) {
  # var.names <- c("v1","v2","v3","v4","v5")
  n.vars <- length(var.names) # number of vars (axes) required
  # Cacluate required number of angles (in radians)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  # calculate vectors of min and max x+y coords
  min.x <- min * sin(angles)
  min.y <- min * cos(angles)
  max.x <- max * sin(angles)
  max.y <- max * cos(angles)
  # Combine into a set of uniquely numbered paths (one per variable)
  axisData <- NULL
  for (i in 1:n.vars) {
    a <- c(i, min.x[i], min.y[i])
    b <- c(i, max.x[i], max.y[i])
    axisData <- rbind(axisData, a, b)
  }
  # Add column names + set row names = row no. to allow conversion into a data frame
  colnames(axisData) <- c("axis.no", "x", "y")
  rownames(axisData) <- seq(1:nrow(axisData))
  # Return calculated axis paths
  as.data.frame(axisData)
}

funcCircleCoords <- function(center = c(0, 0), r = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

generate_color_values <- function(num_groups) {
  # Fallback colors for 1 or 2 groups
  fallback_colors <- c("#E41A1C", "#377EB8") # Adjust these colors as needed

  if (num_groups == 1) {
    # Return the first color if only one group is requested
    return(fallback_colors[1])
  } else if (num_groups == 2) {
    # Return the first two colors for two groups
    return(fallback_colors[1:2])
  } else if (num_groups <= max(RColorBrewer::brewer.pal.info$maxcolors)) {
    # Use RColorBrewer for 3 to max colors
    return(RColorBrewer::brewer.pal(num_groups, "Set3"))
  } else {
    # For more than the maximum supported colors in RColorBrewer, use a color ramp
    return(colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(num_groups))
  }
}


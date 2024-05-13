#' Timeseries_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Timeseries_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class='top_border',
        column(1),
        column(10,  uiOutput(ns('plot'))
        ),
        column(1)
    )
  )
}

#' Timeseries_overall Server Functions
#'
#' @noRd
mod_Timeseries_overall_server <- function(id, i18n, filtered_slick,
                                          pm_ind, yrange,
                                          window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot1 <- renderPlot({
      req(pm_ind())
      req(yrange())
      Timeseries_plot(filtered_slick(), pm_ind())
    })

    output$plot <- renderUI({
      tagList(
        br(),
        shinycssloaders::withSpinner(
          plotOutput(ns('plot1'), width=plot_width(), height=plot_height())
        )
      )

    })

    plot_width_calc <- reactive({
      dd <- window_dims()
      val <- dd[1] * 0.6
      paste0(val, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    plot_height_calc <- reactive({
      dd <- window_dims()
      val <- dd[2] * 0.6
      paste0(val, 'px')
    })

    plot_height <- plot_height_calc |> debounce(500)



  })
}


Timeseries_plot <- function(slick, pm_ind=1,
                            mp_ind=NULL,
                            om=NULL,
                            sims=NULL) {

  slick <<- slick
  pm_ind <<- pm_ind
  mp_ind <<- mp_ind
  om <<- om
  sims <<- sims

  if (length(pm_ind)<1) return(NULL)

  timeseries <- Timeseries(slick)
  values <- Value(timeseries)
  mp_metadata <- slick |> MPs() |> Metadata()
  nMP <- nrow(mp_metadata)
  times <- Time(timeseries)
  time_lab <- names(times)[[1]]

  metadata <- Metadata(timeseries)
  PM_lab <- metadata$Label[pm_ind]


  first.yr <- times[[1]][1]
  periods <- unique(times$Period)
  if (length(periods)!=2)
    stop('Must have two unique values in `Period` column in Time dataframe')

  if(is.null(om))
    om <- 1:dim(values)[2]


  p <- ggplot2::ggplot() +  ggplot2::theme_classic()

  hist.yr.ind <- which(times$Period==periods[1]) |> max()
  hist.yr <-  times[[1]][hist.yr.ind]
  all.hist.yr <- times[[1]][seq_len(hist.yr.ind)]
  n.yrs <- length(times[[1]])
  proj.yr.ind <- hist.yr.ind:n.yrs
  all.proj.yr <- times[[1]][hist.yr.ind:n.yrs]

  if (is.null(mp_ind)) {
    # Historical
    med.hist <- apply(values[,om,1, pm_ind,1:hist.yr.ind, drop=FALSE], 5, median)
    quant.1.hist <- apply(values[,om,1,pm_ind,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=c(.25, .75))
    quant.2.hist <- apply(values[,om,1, pm_ind,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=c(.1, .9))

    meddf <- data.frame(x=all.hist.yr, median=med.hist)

    quant1df <- data.frame(x=all.hist.yr,
                           Lower=quant.1.hist[1,],
                           Upper=quant.1.hist[2,])

    quant2df <- data.frame(x=all.hist.yr,
                           Lower=quant.2.hist[1,],
                           Upper=quant.2.hist[2,])


    p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower, ymax=Upper), data=quant2df,
                                  color='#c9c9c9', fill='white', linetype='dashed') +
      ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower, ymax=Upper), data=quant1df,
                           fill='#ededed') +
      ggplot2::geom_line(ggplot2::aes(x=x, y=median), data=meddf,
                         color='darkgray')
  }

  # projection
  med.mps <- apply(values[,om,, pm_ind,proj.yr.ind, drop=FALSE], c(3,5), median)

  meddf <- data.frame(x=rep(all.proj.yr, each=nMP),
                      MP=mp_metadata$Code,
                      median=as.vector(med.mps))

  meddf$MP <- factor(meddf$MP, ordered=TRUE, levels=mp_metadata$Code)


  if (!is.null(mp_ind)) {
    PM_lab <- ''
    meddf <- meddf |> dplyr::filter(MP%in%mp_metadata$Code[mp_ind])

    quant.1.mp <- apply(values[om,,mp_ind, pm_ind,proj.yr.ind, drop=FALSE], c(3,5), quantile, probs=c(.25, .75))
    quant.2.mp <- apply(values[om,,mp_ind, pm_ind,proj.yr.ind, drop=FALSE], c(3,5), quantile, probs=c(.1, .9))

    quant1df <- data.frame(x=rep(all.proj.yr, each=1),
                           MP=mp_metadata$Code[mp_ind],
                           Lower=as.vector(quant.1.mp[1,,]),
                           Upper=as.vector(quant.1.mp[2,,]))

    quant1df$MP <- factor(quant1df$MP, ordered=TRUE, levels=mp_metadata$Code)

    quant2df <- data.frame(x=rep(all.proj.yr, each=1),
                           MP=mp_metadata$Code[mp_ind],
                           Lower=as.vector(quant.2.mp[1,,]),
                           Upper=as.vector(quant.2.mp[2,,]))

    quant2df$MP <- factor(quant2df$MP, ordered=TRUE, levels=mp_metadata$Code)

    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower, ymax=Upper), data=quant2df,
                           color='#c9c9c9', fill='white', linetype='dashed') +
      ggplot2::geom_ribbon(ggplot2::aes(x=x, ymin=Lower, ymax=Upper), data=quant1df,
                           fill='#ededed') +
      ggplot2::geom_line(ggplot2::aes(x=x, y=median, color=MP[mp_ind]), data=meddf) +
      ggplot2::scale_color_manual(values=mp_metadata$Color[mp_ind]) +
      ggplot2::guides(color='none') +
      ggplot2::labs(title=mp_metadata$Label[mp_ind]) +
      ggplot2::theme(plot.title =ggplot2::element_text(colour=mp_metadata$Color[mp_ind],
                     face='bold', hjust = 0.5))
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=x, y=median, color=MP), data=meddf) +
      ggplot2::scale_color_manual(values=mp_metadata$Color)
  }

  if (length(om)==1) {
    PM_lab <- ''
    p <- p +  ggplot2::labs(title=om) +
      ggplot2::theme(plot.title =ggplot2::element_text(colour='#D6501C',
                                                       face='bold',
                                                       hjust = 0.5)) +
      ggplot2::guides(color='none')
  }

  p <- p +
    ggplot2::expand_limits(y=0) +
    ggplot2::labs(x=time_lab, y=PM_lab, color='') +
    ggplot2::scale_y_continuous(label=scales::comma) +
    ggplot2::theme(axis.title = ggplot2::element_text(size=16, face='bold'),
                   axis.text = ggplot2::element_text(size=14),
                   legend.text = ggplot2::element_text(size=14)) +
    ggplot2::scale_x_continuous(expand = c(0, 0))

  p
}


## To be copied in the UI
# mod_Timeseries_overall_ui("Timeseries_overall_1")

## To be copied in the server
# mod_Timeseries_overall_server("Timeseries_overall_1")

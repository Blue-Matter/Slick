#' Boxplot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Boxplot_overall_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('results'))
  )
}

#' Boxplot_overall Server Functions
#'
#' @noRd
mod_Boxplot_overall_server <- function(id, i18n, filtered_slick,
                                       plottype,
                                       nOM, nMP, nPM, parent_session,
                                       window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    plot_width_calc <- reactive({
      width <- nMP()*50
      width <- max(width, 200)
      paste0(width, 'px')
    })

    plot_width <- plot_width_calc |> debounce(500)

    plot_width_text <- reactive({
      paste0('width: ', plot_width(), '; height: 320px;')

    })

    output$selectedtype <- reactive({
      plottype()
    })

    outputOptions(output, "selectedtype", suspendWhenHidden = FALSE)

    output$results <- renderUI({
      tagList(
        conditionalPanel("output.selectedtype=='1'", ns=ns,
                        uiOutput(ns('boxplots'))
        ),
        conditionalPanel("output.selectedtype=='2'", ns=ns,
                         uiOutput(ns('violins'))
        ),
        conditionalPanel("output.selectedtype=='3'", ns=ns,
                         uiOutput(ns('both'))
        )
      )
    })


    output$boxplots <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("boxplot", mm, sep="")
          shinycssloaders::withSpinner(plotOutput(session$ns(plotname),
                                                  width=plot_width(),
                                                  height='300px'))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    output$violins <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("violin", mm, sep="")
          shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width=plot_width(),
                                                  height='300px'))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })

    output$both <- renderUI({
      if (!is.null(make_plots())) {
        plot_output_list <- lapply(1:nPM(), function(mm) {
          plotname <- paste("both", mm, sep="")
          shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width=plot_width(),
                                                  height='300px'))
        })
        plot_output_list$cellArgs=list(style = plot_width_text())
        do.call(flowLayout, plot_output_list)
      }
    })


    make_plots <- reactive({

      req(filtered_slick())
      if (is.null(filtered_slick())) {
        return(NULL)
      }
      if (is.na(nPM()))
        return(NULL)
      dd <- filtered_slick() |> Boxplot() |> Value() |>
        dim()

      plot_list <- list()
      if (dd[4]==nPM()) {
        for (i in 1:nPM()) {
          plot_list[[i]] <- BoxPlot(filtered_slick(), i, 'all')
        }
      }
      plot_list
    })


    observeEvent(make_plots(), {
      thisplot <- make_plots()
      for (i in 1:nPM()) {
        local({
          my_i <- i
          plotname <- paste("boxplot", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[1]]
          })

          plotname <- paste("violin", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[2]]
          })

          plotname <- paste("both", my_i, sep="")
          output[[plotname]] <- renderPlot({
            thisplot[[my_i]][[3]]
          })
        })
      }
    })


  })
}



BoxPlot <- function(slick, pm=1, type=c('boxplot', 'violin', 'both', 'all'), byOM=FALSE) {
  boxplot <- slick |> Boxplot()
  values <- boxplot |> Value()
  dd <- dim(values)

  if (pm > dd[4])
    return(NULL)

  Val <- values[,,,pm]

  mp_metadata <- slick |> MPs() |> Metadata()
  mp_names <- mp_metadata$Label
  pm_names <- slick |> Boxplot() |> Metadata() |> dplyr::pull('Label')

  df <- data.frame(Sim=1:dd[1],
                   OM=rep(1:dd[2], each=dd[1]),
                   MP=rep(mp_names, each=dd[1]*dd[2]),
                   value=as.vector(Val))

  df$MP <- factor(df$MP, ordered=TRUE, levels=mp_names)

  box_df <- data.frame(
    MP = mp_names,
    m = tapply(df$value, df$MP, median),
    low1 = tapply(df$value, df$MP, quantile, 0.25, na.rm = TRUE),
    upp1 = tapply(df$value, df$MP, quantile, 0.75, na.rm = TRUE),
    low2 = tapply(df$value, df$MP, min, na.rm = TRUE),
    upp2 = tapply(df$value, df$MP, max, na.rm = TRUE)
  )
  box_df$MP <- factor(box_df$MP, ordered=TRUE, levels=mp_names)


  p <- ggplot2::ggplot(df, ggplot2::aes(x=MP, color=MP, fill=MP)) +
    ggplot2::scale_fill_manual(values=mp_metadata$Color) +
    ggplot2::scale_color_manual(values=mp_metadata$Color) +
    ggplot2::guides(color='none', fill='none')    +
    ggplot2::labs(x='', y='', title=pm_names[pm]) +
    ggplot2::expand_limits(y=c(0,100)) +
    ggplot2::coord_cartesian(clip = 'off') +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(legend.position='none',
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text=ggplot2::element_text(size=16),
                   plot.title = ggplot2::element_text(face="bold"),
                   axis.ticks.x=ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size=14, color='#D6501C'),
                   strip.background=ggplot2::element_rect(fill=NA)
                   )

  p1 <- p + ggplot2::geom_linerange(data = box_df,
                                    ggplot2::aes(x=MP, ymin=low2, ymax=upp2, color=MP)) +
    ggplot2::geom_pointrange(data = box_df,
                             ggplot2::aes(x=MP, y=m, ymin=low1, ymax=upp1, color=MP, fill=MP),
                             linewidth = 2, shape = 21, inherit.aes = FALSE, size=1.5)

  if (byOM)
    p1 <- p1 + ggplot2::facet_wrap(~OM)

  p2 <- p + ggplot2::geom_violin(scale='width', ggplot2::aes(y=value))

  if (byOM)
    p2 <- p2 + ggplot2::facet_wrap(~OM)


  p3 <- p +
    ggplot2::geom_violin(scale='width', ggplot2::aes(y=value)) +
    ggplot2::geom_linerange(data = box_df, color='black',
                            ggplot2::aes(x=MP, ymin=low2, ymax=upp2)) +
    ggplot2::geom_pointrange(data = box_df,
                             ggplot2::aes(x=MP, y=m, ymin=low1, ymax=upp1),
                             linewidth = 2, shape = 21, inherit.aes = FALSE, size=1)

  if (byOM)
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


## To be copied in the UI
# mod_Boxplot_overall_ui("Boxplot_overall_1")

## To be copied in the server
# mod_Boxplot_overall_server("Boxplot_overall_1")

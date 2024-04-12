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
    uiOutput(ns('page'))

  )
}

#' Boxplot_overall Server Functions
#'
#' @noRd
mod_Boxplot_overall_server <- function(id, i18n, filtered_slick,
                                       nOM, nMP, nPM, parent_session,
                                       window_dims){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$page <- renderUI({
      # print('nOM')
      # print(nOM())
      #
      # print('nMP')
      # print(nMP())
      #
      # print('nPM')
      # print(nPM())

      vals <- filtered_slick() |> Boxplot() |> Value()

      i18n <- i18n()
      tagList(
        fluidRow(
          column(3,
                 h4(strong(i18n$t("Reading this Chart"))),
                 htmlOutput(ns('reading'))
          ),
          column(9,
                 uiOutput(ns('results'))
                 )
        )
      )
    })

    plot_width <- reactive({
      paste0(nMP()*40, 'px')
    })

    plot_width_text <- reactive({
      paste0('width: ', plot_width(), ';')

    })

    output$results <- renderUI({
      plot_output_list <- lapply(1:nPM(), function(mm) {
        plotname <- paste("plot", mm, sep="")
        shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width=plot_width()))
      })
      plot_output_list$cellArgs=list(style = plot_width_text())
      do.call(flowLayout, plot_output_list)
    })

    observe({
      if (!is.null(filtered_slick())) {
        dd <- filtered_slick() |> Boxplot() |> Value() |>
          dim()

        if (dd[4]==nPM()) {
          for (i in 1:nPM()) {
            local({
              my_i <- i
              plotname <- paste("plot", my_i, sep="")
              output[[plotname]] <- renderPlot({
                BoxPlot_PM(filtered_slick(), my_i)
              })
            })
          }
        }
      }
    })


    observeEvent(input$openfilter, {
      shinydashboardPlus::updateBoxSidebar('filtersidebar', session=parent_session)
    })

    output$reading <- renderUI({
      i18n <- i18n()
      mp_metadata <- filtered_slick() |> MPs() |> Metadata()

      icon_text <- paste('<i class="fas fa-circle fa-sm" style="color:', mp_metadata$Color, ';"></i>', mp_metadata$Label, '<br/>')
      icon_text <- paste(icon_text, collapse=" ")
      tagList(
        p(strong('Key'), br(),
          i18n$t('Management Procedure'),
          br(),
          HTML(icon_text)),
        p(
          i18n$t('This chart compares the performance of '), nMP(),
          i18n$t(' management procedures (MP) across '), nOM(),
          i18n$t(' operating models.')
          ),
        p(i18n$t('All performance indicators are defined such that higher values mean better performance and lower values mean worse performance')
          ),
        img(src='www/img/Boxplot.jpg', width='100%')
      )
    })

  })
}

BoxPlot <- function(slick, lang=NULL) {

  boxplot <- slick |> Boxplot()
  values <- boxplot |> Value()



}

BoxPlot_PM <- function(slick, pm) {
  boxplot <- slick |> Boxplot()
  values <- boxplot |> Value()
  dd <- dim(values)
  if (pm <= dd[4]) {
    Val <- values[,,,pm]

    mp_metadata <- slick |> MPs() |> Metadata()
    pm_metadata <- slick |> Boxplot() |> Metadata()

    med.cex <- 2.5
    qrt.lwd <- 4
    rng.lwd <- 1
    main.cex <- 1.25

    dd <- dim(Val)
    nMPs <- dd[3]

    med <- apply(Val,3, median, na.rm=TRUE)
    qrt <- apply(Val,3, quantile, c(0.25, 0.75), na.rm=TRUE)
    rng <- apply(Val,3, range, na.rm=TRUE)
    maxY <- max(rng[is.finite(rng)])
    if (maxY > 10 & maxY<=100) {
      maxY <- 10^(ceiling(log10(maxY)))
    } else if (maxY <10) {
      maxY <- ceiling(maxY)
    } else if (maxY > 100) {
      maxY <- round(maxY/100)*100
    } else {
      maxY <- round(maxY/1000)*1000
    }

    par(mfrow=c(1,1), oma=c(2,2,3,0), mar=c(1,2,1,1))
    ylim <- pretty(c(0, maxY))
    ymax <- max(ylim)
    plot(c(0,nMPs+1), range(ylim), type='n', axes=FALSE, ylab='', xlab='')
    polygon(c(0, nMPs+1, nMPs+1, 0),
            c(0, 0, ymax, ymax),
            col='#ededed', border=NA)
    at <- ylim
    abline(h=at, col='white')
    text(0, at, at, col='#8c8c8c', xpd=NA, cex=1.25, pos=2)
    points(1:nMPs, med, pch=16, col=mp_metadata$Color, cex=med.cex, xpd=NA)
    for (i in 1:nMPs) {
      lines(c(i,i), qrt[,i], col=mp_metadata$Color[i], lwd=qrt.lwd)
      lines(c(i,i), rng[,i], col=mp_metadata$Color[i], lwd=rng.lwd)
    }
    mtext(side=3, line=0, pm_metadata$Label[pm], font=2, cex=main.cex,
          adj=0)
  }

}


## To be copied in the UI
# mod_Boxplot_overall_ui("Boxplot_overall_1")

## To be copied in the server
# mod_Boxplot_overall_server("Boxplot_overall_1")

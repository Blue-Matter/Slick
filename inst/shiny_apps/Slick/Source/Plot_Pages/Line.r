# This is page 6 of the Plot_Finals.pdf

LineServer <- function(id, MPkeep, SNkeep, Object, i18n) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- renderUI({
                   if(!Object$Loaded) {
                     return(
                       tagList(
                         box(title=i18n()$t('Slick Data File not loaded.'), status='danger',
                             solidHeader = TRUE,
                             p(
                               i18n()$t('Please return to'), a('Load', onclick='customHref("load")',
                                                               style='color:blue; cursor: pointer;'),
                               i18n()$t('and load a Slick file')
                             )
                         )
                       )
                     )
                   }
                 })

                 output$title <- renderUI({
                   tagList(
                     div(class='page_title',
                         h3(i18n()$t('Line'), id='title')
                     )
                   )
                 })

                 output$subtitle <- renderUI({
                   if(!Object$Loaded) return()
                   Object$obj$StateVar$Values
                   first.yr <- Object$obj$StateVar$Times[1]
                   hist.yr <- Object$obj$StateVar$TimeNow
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   if (n.OM>0 & n.MP>0) {
                     str <- paste0(n.MP, ' management procedures. ',
                                   'Median values over ', n.OM,  ' operating models.')

                   } else {
                     str <-''
                   }
                   tagList(
                     div(class='page_title',
                         p(str, id='subtitle')
                     )
                   )
                 })

                 dummyKeep <- list()
                 dummyKeep$selected <- 1:3
                 dummyDet <- list()
                 summaryServer('page5', dummyDet, MPkeep, dummyKeep, SNkeep, Object,
                               page_5_summary, minPMs=3, input)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   n.sim <- dim(Object$obj$StateVar$Values)[1]
                   if (n.OM ==1) {
                     txt <- HTML(paste0(' for a ', strong('single '), 'Operating Model.'))
                   } else {
                     txt <- HTML(paste0(' and across ', strong(n.OM), ' Operating Models.'))
                   }

                   if (n.MP>0 & n.OM>0) {
                     tagList(
                       p(
                         'This chart compares ',
                         strong('projected stock status variables over time'), '(selectable under State Variable dropdown at right)',
                         strong('for ',
                                n.MP, ' management procedures'),
                         'and level of ', strong('uncertainty'),
                         'across', n.sim, 'different simulation runs',
                         txt),
                       p('Target and limit reference points are shown in green and red, respectively, if they have been specified.')
                     )
                   }
                 })


                 output$SV_dropdown <- renderUI({
                   if(!Object$Loaded) return()
                   tagList(
                     h4('Select State Variable for Projection'),
                     selectInput(session$ns('selectSV'),
                                 'State Variable',
                                 choices=Object$obj$StateVar$Labels)
                   )
                 })


                 output$SV_Yrange <- renderUI({
                   ymax <- maxVal <- 4
                   if(!Object$Ready) return()

                   SV_ind <- unlist(Object$obj$StateVar$Labels) == input$selectSV

                   nSNs <- sum(SNkeep$selected) # number SN selected
                   nMPs <- sum(MPkeep$selected) # n MPs selected
                   nSVs <- sum(SV_ind)

                   if (nMPs>0 & nSVs>0 & nSNs>0) {
                     maxVal <- quantile(Object$obj$StateVar$Values[,SNkeep$selected, MPkeep$selected, SV_ind, , drop=FALSE], 0.95)
                     ymax <- roundUpNice(maxVal)
                   }

                   tagList(
                     h4('Set Y Axis'),
                     sliderInput(session$ns('yaxis'),
                                 'Range',
                                 0,
                                 ymax,
                                 c(0, maxVal))
                   )
                 })


                 output$main_plot <- renderPlot({
                   if(!Object$Loaded) return()
                   Stock_Projection_all(MPkeep, SNkeep, input, Object$obj)
                 })


                 output$mp_plots <- renderUI({
                   if(!Object$Loaded) return()
                   nMPs <- sum(MPkeep$selected)
                   plot_output_list <- lapply(1:nMPs, function(mm) {
                     plotname <- paste("plot", mm, sep="")
                     shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width=250))
                   })
                   do.call(flowLayout , plot_output_list)
                 })

                 observe({
                   nMPs <- sum(MPkeep$selected)
                   for (i in 1:nMPs) {
                     local({
                       my_i <- i
                       plotname <- paste("plot", my_i, sep="")
                       output[[plotname]] <- renderPlot({
                         MP_projection(MPkeep, SNkeep, input, mm=my_i, Object$obj)
                       })
                     })
                   }
                 })


               })
}



LineUI <- function(id, label="line") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             htmlOutput(ns('title')),
             htmlOutput(ns('checkloaded')),

             conditionalPanel('output.Loaded>0',
                              htmlOutput(ns('subtitle'))
             )
      )
    ),
    conditionalPanel('output.Loaded>0',
                     fluidRow(
                       column(width = 5,
                              div(
                                summaryUI(ns('page5'))
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 3, class='page_reading',
                              h4(strong("READING THIS CHART")),
                              htmlOutput(ns('reading'))
                       ),
                       column(width=3,
                              br(),
                              img(src='img/Line.jpg', width="100%")
                       ),
                       column(width=1,
                       ),
                       column(width=4,
                              br(),
                              htmlOutput(ns('SV_dropdown')),
                              br(),
                              htmlOutput(ns('SV_Yrange'))
                       )
                     ),
                     fluidRow(
                       column(width=4, class='top_border',
                              shinycssloaders::withSpinner(plotOutput(ns('main_plot')))
                              # hover = ns("plot_hover"),
                              # height='450px'),
                       ),
                       column(width=8, class='top_border',
                              uiOutput(ns('mp_plots'))#,
                              # hover = ns("plot_hover"),
                              # height='450px')
                       )
                     )
    )
  )
}

# projection plot
Stock_Projection_all <- function(MPkeep, SNkeep, input, obj) {
  SV_ind <- which(obj$StateVar$Labels == input$selectSV )
  Values <- obj$StateVar$Values[,SNkeep$selected, MPkeep$selected,
                                SV_ind, , drop=FALSE]

  ylab.cex <- xlab.cex <- 1.25
  med.lwd <- 4
  quant.lwd <- 2
  med.col <- 'darkgray'
  poly.col <- '#ededed'
  quant.col <- '#c9c9c9'
  ref.pt.1.col <- 'green'
  ref.pt.2.col <- 'red'

  first.yr <- obj$StateVar$Times[1]
  hist.yr <- obj$StateVar$TimeNow
  hist.yr.ind <- which(obj$StateVar$Times==hist.yr)
  last.proj <- obj$StateVar$Times[length(obj$StateVar$Times)]
  first.proj <- obj$StateVar$Times[hist.yr.ind+1]
  n.yrs <- dim(Values)[5]

  all.hist.yr <- obj$StateVar$Times[seq_len(hist.yr.ind)]
  all.proj.yr <- obj$StateVar$Times[hist.yr.ind:n.yrs]

  if (!any(dim(Values)==0)) {
    med.hist <- apply(Values[,,1,1,1:hist.yr.ind, drop=FALSE], 5, median)
    quant.1.hist <- apply(Values[,,1,1,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=c(.25, .75))
    quant.2.hist <- apply(Values[,,1,1,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=c(.1, .9))

    med.mps <- apply(Values[,,,1,(hist.yr.ind):n.yrs, drop=FALSE], c(3,5), median)

    #maxVal <- max(c(quant.2.hist, med.mps))
    #ymax <- roundUpNice(maxVal)
    yrange <- input$yaxis
    ymax <- max(yrange)

    par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(3,5,2,1), xaxs="i", yaxs='i')
    plot(range(obj$StateVar$Times), yrange, #c(0, ymax),
         xlab='', ylab='', axes=FALSE, type="n")

    if (all(quant.1.hist != med.hist)) { # values differ by sim
      polygon(x=c(all.hist.yr, rev(all.hist.yr)),
              y=c(quant.1.hist[1,], rev(quant.1.hist[2,])),
              border=NA, col=poly.col)
      lines(all.hist.yr, quant.2.hist[1,], lty=3, col=quant.col, lwd=quant.lwd)
      lines(all.hist.yr, quant.2.hist[2,], lty=3, col=quant.col, lwd=quant.lwd)
    }
    lines(all.hist.yr, med.hist, lwd=med.lwd, col=med.col)


    # Reference Points
    if (length(obj$StateVar$RefPoints)>0 & length(obj$StateVar$RefPoints)>=SV_ind) {
      RefPoints <- obj$StateVar$RefPoints[[SV_ind]]
      if (!all(is.na(RefPoints))) {
        # ref points exist
        lines(c(first.yr,last.proj), rep(RefPoints[1],2), col=ref.pt.1.col)
        text(quantile(obj$StateVar$Times, 0.15), RefPoints[1],
             paste0(obj$StateVar$RefNames[[SV_ind]][1], ' reference point'),
             col=ref.pt.1.col, pos=3, xpd=NA)

        lines(c(first.yr,last.proj), rep(RefPoints[2],2), col=ref.pt.2.col)
        text(quantile(obj$StateVar$Times, 0.15), RefPoints[2],
             paste0(obj$StateVar$RefNames[[SV_ind]][2], ' reference point'),
             col=ref.pt.2.col, pos=1, xpd=NA)
      }
    }
    # Projection - medians


    MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
    MPnames <- obj$MP$Labels[MPkeep$selected] # MP names
    pos <- 1
    for (mm in seq_along(MPnames)) {
      lines(all.proj.yr, med.mps[mm,], col=MPcols[mm], lwd=med.lwd)
      lab.yr <- quantile(all.proj.yr, 0.9)
      ind <- which.min(lab.yr/all.proj.yr)
      text(lab.yr, med.mps[mm,ind], MPnames[mm], col=MPcols[mm], pos=pos, xpd=NA)
      if (pos==1) {
        pos <- 3
      } else if (pos==3) pos <-1
    }

    # Axes and labels
    axis(side=1, at=pretty(obj$StateVar$Times))
    ylabs <- seq(min(yrange), max(yrange), length.out=6)

    axis(side=2, las=1, at=ylabs, label= format(ylabs, big.mark = ",", scientific = FALSE))
    mtext(side=1, line=3, obj$StateVar$Time_lab, cex=xlab.cex)
    mtext(side=2, line=5, obj$StateVar$Labels[SV_ind], cex=ylab.cex)
    abline(v=obj$StateVar$TimeNow, lty=3, col='darkgray')

    legend(first.yr, ymax*1.1 ,legend=c('HISTORICAL',paste0(first.yr, '-', hist.yr)),
           bty='n',text.font=2, xpd=NA)

    legend(hist.yr, ymax*1.1, legend=c('PROJECTION',paste0(first.proj, '-', last.proj)),
           bty='n',text.font=2, xpd=NA)
   # text(first.yr, ymax*1.05, paste0('HISTORICAL: ', first.yr, '-', hist.yr),
  #       font=2, pos=4, xpd=NA)
    #text(hist.yr, ymax*0.95, paste0('PROJECTION: ', hist.yr+1, '-', last.proj),
     #    font=2, pos=4, xpd=NA)
  }


}

MP_projection <- function(MPkeep, SNkeep, input, mm=my_i, obj) {

  SV_ind <- which(obj$StateVar$Labels == input$selectSV )

  Values <- obj$StateVar$Values[,SNkeep$selected, MPkeep$selected,
                                SV_ind, , drop=FALSE]

  MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Labels[MPkeep$selected] # MP names

  ylab.cex <- xlab.cex <- 1.25
  med.lwd <- 4
  med.col <- 'darkgray'
  poly.col <- '#ededed'
  quant.col <- '#c9c9c9'
  ref.pt.1.col <- 'green'
  ref.pt.2.col <- 'red'
  quant.lwd <- 2

  first.yr <- obj$StateVar$Times[1]
  hist.yr <- obj$StateVar$TimeNow
  hist.yr.ind <- which(obj$StateVar$Times==hist.yr) + 1
  first.proj <- obj$StateVar$Times[hist.yr.ind]
  last.proj <- obj$StateVar$Times[length(obj$StateVar$Times)]
  n.yrs <- length(obj$StateVar$Times)

  all.proj.yr <- obj$StateVar$Times[hist.yr.ind:n.yrs]

  if (!any(dim(Values)==0)) {
    dd <- dim(Values)[3]
    if (mm <= dd) {
      med.mps <- apply(Values[,,,1,hist.yr.ind:n.yrs, drop=FALSE], c(3,5), median)
      quant.1.proj <- apply(Values[,,,1,hist.yr.ind:n.yrs, drop=FALSE], c(3,5),
                            quantile, probs=c(.25, .75))
      quant.2.proj <- apply(Values[,,,1,hist.yr.ind:n.yrs, drop=FALSE], c(3,5),
                            quantile, probs=c(.1, .9))

      med.hist <- apply(Values[,,1,1,1:hist.yr.ind, drop=FALSE], 5, median)
      quant.1.hist <- apply(Values[,,1,1,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=c(.25, .75))
      quant.2.hist <- apply(Values[,,1,1,1:hist.yr.ind, drop=FALSE], 5, quantile, probs=c(.1, .9))

      med.mps <- apply(Values[,,,1,(hist.yr.ind):n.yrs, drop=FALSE], c(3,5), median)

      #maxVal <- max(c(quant.2.hist, med.mps))
      #ymax <- roundUpNice(maxVal)
      yrange <- input$yaxis

      par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(3,5,2,4), xaxs="i", yaxs='i', xpd=NA)

      plot(c(first.proj, last.proj), yrange, #c(0, ymax),
           xlab='', ylab='', axes=FALSE, type="n")

      polygon(x=c(all.proj.yr, rev(all.proj.yr)),
              y=c(quant.1.proj[1, mm,], rev(quant.1.proj[2, mm,])),
              border=NA, col=poly.col)
      lines(all.proj.yr, quant.2.proj[1,mm,], lty=3, col=quant.col, lwd=quant.lwd)
      lines(all.proj.yr, quant.2.proj[2,mm,], lty=3, col=quant.col, lwd=quant.lwd)
      lines(all.proj.yr, med.mps[mm,], lwd=med.lwd, col=MPcols[mm])

      # Axes and labels

      axis(side=1, at=seq(first.proj, last.proj, by=5))
      ylabs <- seq(min(yrange), max(yrange), length.out=6)

      axis(side=2, las=1, at=ylabs, label= FALSE)
      mtext(side=1, line=3, obj$StateVar$Time_lab, cex=xlab.cex)
      # mtext(side=2, line=4, obj$StateVar$Labels[SV_ind], cex=ylab.cex)

      # Reference Points
      if (length(obj$StateVar$RefPoints)>0 & length(obj$StateVar$RefPoints)>=SV_ind) {
        RefPoints <- obj$StateVar$RefPoints[[SV_ind]]
        if (!all(is.na(RefPoints))) {
          # ref points exist
          lines(c(first.proj,last.proj), rep(RefPoints[1],2), col=ref.pt.1.col)
          text(quantile(obj$StateVar$Times, 0.15), RefPoints[1],
               paste0(obj$StateVar$RefNames[[SV_ind]][1], ' reference point'),
               col=ref.pt.1.col, pos=3)

          lines(c(first.proj,last.proj), rep(RefPoints[2],2), col=ref.pt.2.col)
          text(quantile(obj$StateVar$Times, 0.15), RefPoints[2],
               paste0(obj$StateVar$RefNames[[SV_ind]][2], ' reference point'),
               col=ref.pt.2.col, pos=1)
        }
      }

      title(MPnames[mm], col.main=MPcols[mm], line=0)
    }
  }
}

page_5_summary <- function(dummyDet, MPkeep, dummyKeep, SNkeep, Object,
                           input) {

  obj <- Object$obj

  SV_ind <- which(obj$StateVar$Labels == input$selectSV )

  Values <- obj$StateVar$Values[,SNkeep$selected, MPkeep$selected,
                                SV_ind, , drop=FALSE]


  if (!any(dim(Values)==0)) {

    first.yr <- obj$StateVar$Times[1]
    hist.yr <- obj$StateVar$TimeNow
    hist.yr.ind <- which(obj$StateVar$Times==hist.yr)
    last.proj <- obj$StateVar$Times[length(obj$StateVar$Times)]
    n.yrs <- length(obj$StateVar$Times)
    med.mps <- apply(Values[,,,,hist.yr.ind:n.yrs, drop=FALSE], 3, median)

    MPnames <- obj$MP$Labels[MPkeep$selected]

    Highest <- paste('MP with highest median value over projection period:',
                     paste(MPnames[which.max(med.mps)], collapse=", "))

    Lowest <- paste('MP with lowest median value over projection period:',
                     paste(MPnames[which.min(med.mps)], collapse=", "))

    return(paste0(c(Highest, Lowest), collapse = '\n'))
  }
}

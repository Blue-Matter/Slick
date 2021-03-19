# ---- Page 11 - Projections ----

# This is page 12 of Plot_Finals.pdf

LineOMServer <- function(id, MPkeep, SNkeep, Object) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- CheckLoaded(Object)

                 output$subtitle <- renderUI({
                   if(!Object$Ready) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   if (n.OM>0 & n.MP>0) {
                     str <- paste0(n.MP, ' management procedures. ',
                                   'Median values over ', n.OM,  ' operating models.')
                   } else {
                     str <-''
                   }
                   tagList(
                     p(str, id='subtitle')
                   )
                 })

                 dummyKeep <- list()
                 dummyKeep$selected <- 1:3
                 dummyDet <- list()
                 summaryServer('lineOM', dummyDet, MPkeep, dummyKeep, SNkeep, Object,
                               lineOM_summary, minPMs=3, input)


                 output$reading <- renderUI({
                   if(!Object$Ready) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)

                   n.sim <- dim(Object$obj$StateVar$Values)[1]

                   if (n.MP>0 & n.OM>0) {
                     tagList(
                       p(
                         'This chart compares ',
                         strong('projected stock status variables over time (selectable under State Variable dropdown at right) for ',
                                n.MP, ' management procedures'),
                         'in ', n.sim, 'simulations by', strong(
                           n.OM, 'operating models.'
                         )
                       ),
                       p('Target and limit reference points are shown in green and red, respectively, if they have been specified.')
                     )
                   }
                 })

                 output$SV_dropdown <- renderUI({
                   if(!Object$Ready) return()
                   tagList(
                     h4('Select State Variable for Projection'),
                     selectInput(session$ns('selectSV'),
                                 'State Variable',
                                 choices=Object$obj$StateVar$Labels)
                   )
                 })

                 output$main_plot <- renderPlot({
                   if(!Object$Ready) return()
                   Stock_Projection_hist(MPkeep, SNkeep, input, Object$obj)
                 }, height=400)


                 output$mp_plots <- renderUI({
                   if(!Object$Ready) return()
                   n.SN <- sum(SNkeep$selected)
                   SN.select <- which(SNkeep$selected)
                   plot_output_list <- lapply(1:n.SN, function(mm) {
                     plotname <- paste("plot", mm, sep="")
                     tagList(
                       plotOutput(session$ns(plotname), width='150px', height='400px')
                     )

                   })
                   plot_output_list$cellArgs <- list(
                     style = "
                     width: 150px;
                     height: 450px;
                           "
                   )

                   # tagList(
                   #   fluidRow(
                   #     h3('Operating Models',style="text-align: center;")
                   #   ),
                   #
                   # )
                   do.call(flowLayout, plot_output_list)

                 })

                 observe({
                   n.SN <- sum(SNkeep$selected)
                   SN.select <- which(SNkeep$selected)
                   for (i in 1:length(SNkeep$selected)) {
                     if (i %in% SN.select) {
                       local({
                         my_i <- i
                         plotname <- paste("plot", my_i, sep="")
                         output[[plotname]] <- renderPlot({
                           MP_projection_OM(MPkeep, input, sn=my_i, Object$obj)
                         })
                       })
                     }

                   }
                 })

                 output$MPlist <- renderUI({
                   if(!Object$Ready) return()
                   n.MP <- sum(MPkeep$selected)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names

                   # write css class
                   text <- paste0("<p> <b class='horizline' style=' border-top: .3rem solid", MPcols, ";'></b>",
                                  MPnames, "</p>")

                   text <- paste(text, collapse=" ")

                   tagList(
                     HTML(text)
                   )

                 })

                 output$hist_text <- renderUI({
                   if(!Object$Ready) return()
                   first.yr <- Object$obj$StateVar$Times[1]
                   hist.yr <- Object$obj$StateVar$TimeNow
                   years <- paste(first.yr, hist.yr, sep=" - ")

                   tagList(
                     p(strong('HISTORICAL')),
                     p(years)
                   )

                 })

                 output$proj_text <- renderUI({
                   if(!Object$Ready) return()
                   last.yr <- Object$obj$StateVar$Times[length(Object$obj$StateVar$Times)]
                   hist.yr <- Object$obj$StateVar$TimeNow
                   years <- paste(hist.yr+1, last.yr, sep=" - ")

                   tagList(
                     p(strong('PROJECTIONS')),
                     p(years)
                     )

                 })

               }
  )
}




Line_OMUI <- function(id, label="lineOM") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             div(class='page_title',
                 h3('Stock Projections', id='title'),
                 htmlOutput(ns('checkloaded')),
                 htmlOutput(ns('subtitle'))
             )
      )
    ),
    fluidRow(
      column(width = 8,
             div(
                 summaryUI(ns('lineOM'))
             )
      )
    ),
    fluidRow(
      column(width=3,
             br(),
             h4(strong("Management Procedure")),
             htmlOutput(ns('MPlist'))
      ),
      column(width=3,  class='page_reading',
             h4(strong("READING THIS CHART")),
             htmlOutput(ns('reading'))
             ),
      column(width=3,
             img(src='img/page_11_reading.JPG')
             ),
      column(width=4,
             br(),
             htmlOutput(ns('SV_dropdown'))
      )

    ),
    fluidRow(
      column(width=4, class='top_border',
             br(), br(),
             uiOutput(ns('hist_text')),
             div(
                 plotOutput(ns('main_plot'))
             )
      ),
      column(width=8, class='top_border',
             h4('Operating Models',style="text-align: left;"),
             uiOutput(ns('proj_text')),
             uiOutput(ns('mp_plots'))


      )
    )
  )
}


Stock_Projection_hist <- function(MPkeep, SNkeep, input, obj) {

  SV_ind <- which(unlist(obj$StateVar$Labels) == input$selectSV )

  first.yr <- obj$StateVar$Times[1]
  hist.yr <- obj$StateVar$TimeNow
  hist.yr.ind <- which(obj$StateVar$Times==hist.yr)

  Values <- obj$StateVar$Values[,SNkeep$selected, MPkeep$selected,
                                SV_ind, , drop=FALSE]

  ylab.cex <- xlab.cex <- 1.25
  med.lwd <- 3
  ref.lwd <- 2
  med.col <- 'darkgray'
  poly.col <- 'lightgray'
  ref.pt.1.col <- 'green'
  ref.pt.2.col <- 'red'

  if (!any(dim(Values)==0)) {
    med.hist <- apply(Values[, ,1,1,1:hist.yr.ind, drop=FALSE], 5, median)

    maxVal <- quantile(Values, 0.95)
    if (maxVal <10) {
      ymax <- ceiling(maxVal)
    } else {
      ymax <- 10^ceiling(log10(maxVal))
    }

    par(mfrow=c(1,1), oma=c(3,3,0,0), mar=c(2,2,2,0))
    plot(range(obj$StateVar$Times[1:hist.yr.ind]), c(0, ymax),
         xlab='', ylab='', axes=FALSE, type="n")

    RefNames <- obj$StateVar$RefNames[[SV_ind]]
    RefPoints <- rep('', length(RefNames))
    if (length(obj$StateVar$RefPoints)>0)
      RefPoints <- obj$StateVar$RefPoints[[SV_ind]]

    if (!all(is.na(RefNames))) {
      for (i in seq_along(RefNames)) {
        nm <- RefNames[i]
        col <- 'black'
        if (nm =='Target') col <- 'green'
        if (nm =='Limit') col <- 'red'
        abline(h=RefPoints[i], lty=3, col=col, lwd=ref.lwd)
        text(first.yr+1, RefPoints[i], RefNames[i], col=col, pos=3)
      }
    }
    lines(obj$StateVar$Times[1:hist.yr.ind], med.hist, col=med.col, lwd=med.lwd)
    axis(side=1)
    ylabs <- seq(0, ymax, length.out=6)
    axis(side=2, las=1, at=ylabs, label= format(ylabs, big.mark = ",", scientific = FALSE))
    mtext(side=1, line=3, obj$StateVar$Time_lab, cex=xlab.cex)
    mtext(side=2, line=4, obj$StateVar$Labels[[SV_ind]], cex=ylab.cex)
  }

}


MP_projection_OM <- function(MPkeep, input, sn, obj) {

  ylab.cex <- xlab.cex <- 1.25
  med.lwd <- 4
  ref.lwd <- 2
  med.col <- 'darkgray'
  poly.col <- '#ededed'
  ref.pt.1.col <- 'green'
  ref.pt.2.col <- 'red'

  SV_ind <- which(unlist(obj$StateVar$Labels) == input$selectSV )

  hist.yr.ind <- which(obj$StateVar$Times==obj$StateVar$TimeNow)
  first.proj.ind <- hist.yr.ind+1
  first.proj <- obj$StateVar$TimeNow+1
  last.proj <- obj$StateVar$Times[length(obj$StateVar$Times)]
  last.proj.ind <- which(obj$StateVar$Times==last.proj)

  Values <- obj$StateVar$Values[,sn, MPkeep$selected,
                                SV_ind, first.proj.ind:last.proj.ind, drop=FALSE]
  n.yrs <- dim(Values)[5]

  MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Codes[MPkeep$selected] # MP names
  nMP <- length(MPnames)

  maxVal <- quantile(obj$StateVar$Values[,, MPkeep$selected,
                                    SV_ind, , drop=FALSE], 0.95)
  if (maxVal <10) {
    ymax <- ceiling(maxVal)
  } else {
    ymax <- 10^ceiling(log10(maxVal))
  }

  if (!any(dim(Values)==0)) {
    quant <- apply(Values, 5, quantile, probs=c(.1,.9))
    med.MP <- apply(Values, c(3,5), median)

    par(mfrow=c(1,1), oma=c(3,1,0,0), mar=c(2,2,2,0))
    plot(range(obj$StateVar$Times[first.proj.ind:last.proj.ind]), c(0, ymax),
         xlab='', ylab='', axes=FALSE, type="n")

    # if (any(quant[1,] != med.MP[mm,])) { # values differ by sim
      polygon(x=c(first.proj:last.proj, rev(first.proj:last.proj)),
              y=c(quant[1,], rev(quant[2,])),
              border=NA, col=poly.col)
    # }

    RefNames <- obj$StateVar$RefNames[[SV_ind]]
    RefPoints <- rep('', length(RefNames))
    if (length(obj$StateVar$RefPoints)>0)
      RefPoints <- obj$StateVar$RefPoints[[SV_ind]]
    if (!all(is.na(RefNames))) {
      for (i in seq_along(RefNames)) {
        nm <- RefNames[i]
        col <- 'black'
        if (nm =='Target') col <- 'green'
        if (nm =='Limit') col <- 'red'
        abline(h=RefPoints[i], lty=3, col=col, lwd=ref.lwd)
      }
    }

    axis(side=1, tck=-0.05)
    ylabs <- seq(0, ymax, length.out=6)
    axis(side=2, las=1, at=ylabs, label= FALSE, tck=-0.05 )
    mtext(side=1, line=3, obj$StateVar$Time_lab, cex=xlab.cex)
    mtext(side=3, line=0, sn, cex=1.5, col='#D6501C')

    for (mm in 1:nMP) {
      lines(first.proj:last.proj, med.MP[mm,],
            col=MPcols[mm], lwd=med.lwd)
      # text(last.proj, med.MP[mm,n.yrs], MPnames[mm], col=MPcols[mm], pos=4, xpd=NA)
    }

  }
}


lineOM_summary <- function(dummyDet, MPkeep, dummyKeep, SNkeep, Object,
                           input) {

  nSN <- nrow(Object$obj$OM$Design)
  obj <- Object$obj

  SV_ind <- which(obj$StateVar$Labels == input$selectSV )

  Values <- obj$StateVar$Values[,SNkeep$selected, MPkeep$selected,
                                SV_ind, , drop=FALSE]

  if (!any(dim(Values)==0)) {

    if (sum(MPkeep$selected)<2) {
      return('Please select 2 or more Management Procedures')
    } else {
      first.yr <- obj$StateVar$Times[1]
      hist.yr <- obj$StateVar$TimeNow
      hist.yr.ind <- which(obj$StateVar$Times==hist.yr)
      last.proj <- obj$StateVar$Times[length(obj$StateVar$Times)]
      n.yrs <- length(obj$StateVar$Times)

      med.mps <- apply(Values[,,,,hist.yr.ind:n.yrs, drop=FALSE], 2:3, median)

      MPnames <- obj$MP$Codes[MPkeep$selected]

      selectedOMs <- (1:nSN)[SNkeep$selected]
      str <- NULL
      for (i in seq_along(selectedOMs)) {
        if (all(med.mps[i,]==mean(med.mps[i,]))) {
          mps <- paste0('all equal (', mean(med.mps[i,]),')')
        } else {
          mps <- paste0(MPnames[which(med.mps[i,]==max(med.mps[i,]))])
        }

        str[i] <- paste0(selectedOMs[i], ': ',
                         paste(mps, collapse=", "), ' (', round(max(med.mps[i,]),2), ')')

      }
      return(paste0('MP(s) with highest median value in the projection period for OM:\n',
                    paste0(str, collapse = '\n')))
    }



  }
}

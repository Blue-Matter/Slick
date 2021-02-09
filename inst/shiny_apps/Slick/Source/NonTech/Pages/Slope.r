# This is page 8 of the Plot_Finals.pdf

SlopeServer <- function(id, Proj, MPkeep, Projkeep, SNkeep, Object) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- CheckLoaded(Object)

                 # subtitle - dynamic nMP and nOM
                 output$subtitle <- renderUI({
                   if(!Object$Loaded) return()
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

                 summaryServer('page7', Proj, MPkeep, Projkeep, SNkeep, Object,
                               page_7_summary, minPMs=3, input)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Projkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   yrs <- Object$obj$Perf$Proj$Times
                   yr.txt <- paste0(min(yrs), '-', max(yrs))
                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     tagList(
                       p(
                         'This chart compares trade-offs in ', strong(n.MP,
                                                                      'management procedures (MPs)'),
                         'for ', n.OM,
                         'operating models by measuring two co-dependent performance metrics.',
                         'Similar performing MPs are grouped together.'),
                       p(HTML('<i class="fas fa-circle fa-sm"></i>'),
                         'The ', strong('dots'),
                         'represent the median value for the final year of the projection period',
                         paste0(yr.txt, '.')),
                       p(strong('Dotted lines'),
                         'next to the dots are error bars representing 90th percentiles.')
                     )
                   }
                 })

                 output$PM_dropdown <- renderUI({
                   if(!Object$Loaded) return()
                   Codes <- Object$obj$Perf$Proj$Codes[Projkeep$selected] # PM codes

                   if (length(Codes)>0) {
                     sel1 <- Codes[[1]]
                     sel2 <- Codes[[2]]
                   } else {
                     sel1 <- 1
                     sel2 <- 2
                   }



                   tagList(
                     fluidRow(
                       br(),
                       h4('Select Performance Metrics'),
                       column(width=6,
                              selectInput(session$ns('selectPM1'),
                                          'PM 1',
                                          choices=Codes,
                                          selected=sel1)
                              ),
                       column(width=6,
                              selectInput(session$ns('selectPM2'),
                                          'PM 2',
                                          choices=Codes,
                                          selected=sel2)
                              )
                     )
                   )
                 })

                 output$results <- renderPlot({
                   if(!Object$Loaded) return()
                   trade_plot2(Proj, MPkeep, Projkeep, SNkeep, input, Object$obj)
                 })

                 output$results_ranking <- DT::renderDataTable({
                   if(!Object$Loaded) return()
                   Codes <- Object$obj$Perf$Proj$Codes # PM codes
                   pm1 <- which(Codes==input$selectPM1)
                   pm2 <- which(Codes==input$selectPM2)

                   nSNs <- sum(SNkeep$selected) # number SN selected
                   nMPs <- sum(MPkeep$selected) # n MPs selected
                   nPMds <- sum(Projkeep$selected) # n PM selected

                   dd <- dim(Object$obj$Perf$Proj$Values)
                   n.yrs <- dd[5]

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names

                   med.cex <- 3

                   if (nMPs>0 & nPMds >1 & nSNs>0) {
                     Values <- Proj$mat[,SNkeep$selected, MPkeep$selected,c(pm1, pm2), ,
                                        drop=FALSE]
                     if(!any(dim(Values)<1)) {
                       med1 <- apply(Values[,,,1,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)
                       med2 <- apply(Values[,,,2,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)

                       icon_text <- paste('<i class="fas fa-circle fa-sm" style="color:', MPcols, ';"></i>')

                       DF <- data.frame(icon=icon_text, MP=MPnames, x1=round(med1,2), x2=round(med2,2))
                       colnames(DF)[1] <- ''
                       colnames(DF)[3:4] <- c(Codes[pm1], Codes[pm2])


                       DT::datatable(DF, extensions = c('Responsive', 'Buttons'),
                                     escape = FALSE,
                                     options=list(
                                       dom = 'Bfrtip',
                                       autoWidth=FALSE),
                                     rownames= FALSE)

                     }
                   }

                 })
               }
  )
}



SlopeUI <- function(id, label="slope") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             div(class='page_title',
                 h3('Trade-off Plot', id='title'),
                 htmlOutput(ns('checkloaded')),
                 htmlOutput(ns('subtitle'))
             )
      )
    ),
    fluidRow(
      column(width = 5,
             div(
               summaryUI(ns('page7'))
             )
      )
    ),
    fluidRow(
      column(width=6, class='page_reading',
             h4(strong("READING THIS CHART")),
             htmlOutput(ns('reading'))
      ),
      column(width = 4,
             htmlOutput(ns('PM_dropdown'))
      )
    ),
    fluidRow(
      column(width=12,
             plotOutput(ns('results'),
                        width='100%')
      )
    ),
    fluidRow(class="top_border",
             column(2),
             column(width=9,
             h3('Performance Table', class='lah'),
             DT::dataTableOutput(ns('results_ranking'), width='80%')
      )
    )
  )
}



trade_plot2 <- function(Proj, MPkeep, Projkeep, SNkeep, input, obj) {

  Codes <- unlist(obj$Perf$Proj$Codes) # PM codes
  pm1 <- which(Codes==input$selectPM1)
  pm2 <- which(Codes==input$selectPM2)

  nSNs <- sum(SNkeep$selected) # number SN selected
  nMPs <- sum(MPkeep$selected) # n MPs selected
  nPMds <- sum(Projkeep$selected) # n PM selected

  dd <- dim(obj$Perf$Proj$Values)
  n.yrs <- dd[5]

  MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Codes[MPkeep$selected] # MP names

  med.cex <- 3
  ax.text <- 2
  lwd <- 4
  cex.text <- 1.6
  top.text <- 1.8
  ref.cex <- 1.6


  if (nMPs>0 & nPMds >1 & nSNs>0 & length(pm1>0) & length(pm2)>0) {

    Values <- Proj$mat[,SNkeep$selected, MPkeep$selected,c(pm1, pm2), ,
                       drop=FALSE]
    if(!any(dim(Values)<1)) {
      med1 <- apply(Values[,,,1,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)
      med2 <- apply(Values[,,,2,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)
      quant1 <- apply(Values[,,,1,n.yrs, drop=FALSE],  3,quantile,
                      c(0.05, 0.95), na.rm=TRUE)
      quant2 <-apply(Values[,,,2,n.yrs, drop=FALSE],  3,quantile,
                     c(0.05, 0.95), na.rm=TRUE)
      maxX <- max(quant1)
      maxY <- max(quant2)

      # Ref points
      ref1 <- obj$Perf$Proj$RefPoints[[pm1]]
      ref1_lab <- obj$Perf$Proj$RefNames[[pm1]]
      ref2 <- obj$Perf$Proj$RefPoints[[pm2]]
      ref2_lab <- obj$Perf$Proj$RefNames[[pm2]]

      ref_DF <- data.frame(PM=rep(c(Codes[pm1], Codes[pm2]), each=2),
                           Ref=c(ref1, ref2),
                           Lab=c(ref1_lab, ref2_lab))
      ref_DF$Col <- "red"
      ref_DF$Col[ref_DF$Lab=="Target"] <- "#49ba3a"
#
#       ind1 <- which(ref1_lab=='Target')
#       col1 <- NA
#       ind2 <- which(ref2_lab=='Target')

      rnk1 <- med1-ref1
      rnk2 <- med2-ref2

      if (grepl('FMSY', Codes[pm1])) {
        rnk1 <- -rnk1
      }
      if (grepl('FMSY', Codes[pm2])) {
        rnk2 <- -rnk2
      }
      ord <- apply(cbind(rnk1, rnk2), 1, mean)
      chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
      group <- chunk2(rev(ord), 3)

      DF <- data.frame(Val=c(med1,med2),
                       MP=MPnames,
                       PM=rep(c(Codes[pm1], Codes[pm2]), each=length(MPnames)),
                       x=rep(c(1.2,1.8),each=length(MPnames)),
                       quant1=c(quant1[1,],quant2[1,]),
                       quant2=c(quant1[2,],quant2[2,]),
                       Group=NA)


      for (i in 1:length(group)) {
        ind <- which(ord %in% group[[i]])
        ind2 <- which(DF$MP %in% MPnames[ind])
        DF$Group[ind2] <- i
      }

      par(mfrow=c(1,3), oma=c(3,3,1,0), mar=c(1,1,1,1))
      if (sum(DF$Group==1)>0)
        doplot(1,TRUE, TRUE, quant1, quant2, pm1, pm2, Codes,
               DF, ref_DF, MPnames, MPcols, med.cex, lwd, cex.text, top.text,ax.text,ref.cex)
      if (sum(DF$Group==2)>0)
        doplot(2,FALSE, FALSE, quant1, quant2, pm1, pm2, Codes,
               DF, ref_DF, MPnames, MPcols, med.cex, lwd, cex.text, top.text,ax.text,ref.cex)
      if (sum(DF$Group==3)>0)
        doplot(3,FALSE, FALSE, quant1, quant2, pm1, pm2, Codes,
               DF, ref_DF, MPnames, MPcols, med.cex, lwd, cex.text, top.text,ax.text,ref.cex)

    }
  }
}

doplot <- function(gp=1, inc_text=FALSE, inc_axis=FALSE,
                   quant1, quant2, pm1, pm2, Codes,
                   DF, ref_DF, MPnames, MPcols, med.cex, lwd, cex.text, top.text,
                   ax.text, ref.cex) {
  ylim <- c(0, max(c(quant1, quant2)))
  if (ylim[2]  <10) {
    ylim[2]  <- ceiling(ylim[2] )
  } else {
    ylim[2]  <- 10^ceiling(log10(ylim[2] ))
  }


  at <- seq(0, ylim[2], by=0.1*ylim[2])
  label <- round(at,2)
  plot(c(1.1,1.9), ylim, type="n", axes=FALSE, xlab='', ylab='')
  polygon(x=c(1.2,1.8,1.8,1.2), y=c(0,0, ylim[2], ylim[2]),
          border=NA, col="#ededed")

  abline(h=at, col='white')
  if(inc_axis)
    text(x=rep(1.2,length(at)), y=at, label, col="#8c8c8c", pos=4, cex=ax.text)
  text(1.3, ylim[2]*1.05, Codes[pm1], pos=3,xpd=NA, font=2, cex=top.text)
  text(1.7, ylim[2]*1.05, Codes[pm2], pos=3,xpd=NA, font=2, cex=top.text)

  df <- DF %>% dplyr::filter(Group==gp)
  pos <- 0; inc <- 0
  for (mm in unique(df$MP)) {
    pos <- pos + 1
    if (pos>4) pos <- 1
    df2 <- df %>% dplyr::filter(MP==mm)
    ind <- match(mm, MPnames)
    lines(df2$x, df2$Val, col='white', lwd=lwd+1)
    lines(df2$x, df2$Val, col=MPcols[ind], lwd=lwd)
    points(df2$x, df2$Val, pch=16, cex=med.cex*1.25, col='white')
    points(df2$x, df2$Val, pch=16, cex=med.cex, col=MPcols[ind])


    # text(mean(df2$x), mean(df2$Val), MPnames[ind],
    #      col=MPcols[ind], pos=pos, cex=cex.text,
    #      font=2)
    inc <- inc + .01
    for (i in seq_along(df2$PM)) {
      multi <- 1.01 + inc
      if (df2$x[i]==1.2) multi <- 1-(multi-1)
      lines(rep(df2$x[i]*multi,2), c(df2$quant1[i], df2$quant2[i]),
            lty=3, col=MPcols[ind])
    }
  }
  # add reference point lines
  for (i in unique(ref_DF$PM)) {
    df3 <- ref_DF %>% dplyr::filter(PM==i)
    i2 <- match(i, unique(ref_DF$PM))
    if (i2==1) x <- c(1.2,1.4)
    if (i2==2) x <- c(1.6,1.8)
    for (r in 1:nrow(df3)) {
      lines(x, c(df3[r,]$Ref, df3[r,]$Ref), col=df3[r,]$Col)
      if(inc_text) {
        if (i2==1) {
          text(x[1], df3[r,]$Ref, toupper(df3[r,]$Lab),
               col=df3[r,]$Col, pos=2, xpd=NA, cex=ref.cex)
        }
        # text(x[1], df3[r,]$Ref, df3[r,]$Ref, , col=df3[r,]$Col, pos=4, xpd=NA,
        #      cex=ref.cex)
      }
    }

  }
}



page_7_summary <-  function(Proj, MPkeep, Projkeep, SNkeep, Object, input) {

  obj <- Object$obj

  nSN <- nrow(obj$OM$Design) # number of SN in obj
  nSNs <- sum(SNkeep$selected) # number SN selected

  dd <- dim(obj$Perf$Proj$Values)
  n.yrs <- dd[5]
  nPMd <- dd[4] # number PM in obj
  nPMds <- sum(Projkeep$selected) # n PM selected

  nMP <- length(obj$MP$Labels) # n MPs in obj
  nMPs <- sum(MPkeep$selected) # n MPs selected

  cols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Codes[MPkeep$selected] # MP names

  Codes <- obj$Perf$Proj$Codes # PM codes

  pm1 <- which(Codes==input$selectPM1)
  pm2 <- which(Codes==input$selectPM2)


  if (nMPs>0 & nPMds >1 & nSNs>0 & length(pm1>0) & length(pm2)>0) {

    Values <- Proj$mat[,SNkeep$selected, MPkeep$selected,c(pm1, pm2), ,drop=FALSE] #

    if(!any(dim(Values)<1)) {
      med1 <- apply(Values[,,,1,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)
      med2 <- apply(Values[,,,2,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)

      val <- MPnames[which.max(med1)]
      if (length(val)<1) val <- 'None'
      HighestPM1 <- paste('MP with highest value for', paste0(Codes[pm1], ":"),
                          paste(val, collapse=", "))

      val <- MPnames[which.min(med1)]
      if (length(val)<1) val <- 'None'
      LowestPM1 <- paste('MP with lowest value for', paste0(Codes[pm1], ":"),
                          paste(val, collapse=", "))

      val <- MPnames[which.max(med2)]
      HighestPM2 <- paste('MP with highest value for', paste0(Codes[pm2], ":"),
                          paste(val, collapse=", "))
      val <- MPnames[which.min(med2)]
      if (length(val)<1) val <- 'None'
      LowestPM2 <- paste('MP with lowest value for', paste0(Codes[pm2], ":"),
                         paste(val, collapse=", "))

      return( paste0(c(HighestPM1, LowestPM1, HighestPM2, LowestPM2), collapse = '\n'))
    }
  }
}




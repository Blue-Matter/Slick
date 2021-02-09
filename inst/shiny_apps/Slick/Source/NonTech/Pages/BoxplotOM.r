# This is page 10 of the Plot_Finals.pdf

Boxplot_OMServer <- function(id, Stoch, MPkeep, Stochkeep, SNkeep, Object) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- CheckLoaded(Object)

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

                 summaryServer('page9', Stoch, MPkeep, Stochkeep, SNkeep, Object,
                               page_9_summary, minPMs = 2)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Stochkeep$selected)
                   n.OM <- sum(SNkeep$selected)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names

                   icon_text <- paste('<i class="fas fa-circle fa-sm" style="color:',
                                      MPcols, ';"></i>', MPnames, '<br/>')
                   icon_text <- paste(icon_text, collapse=" ")

                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     tagList(
                       p(strong('Key'), HTML('<br/>'),
                         'Management Procedure', HTML('<br/>'),
                         HTML(icon_text)),
                       p(
                         'This chart', strong('compares'),
                         'performance of ', n.MP,
                         ' different candidate management procedures (MP) showing ',
                         strong('trade-offs.'), n.OM,
                         ' different operating models are compared.'),
                       p(
                         'All performance metrics are defined such that',
                         strong('higher values mean better performance'),
                         'and',
                         HTML(paste0(strong('lower values mean worse performance'),'.'))
                       )
                     )
                   }
                 })

                 output$results <- renderUI({
                   if(!Object$Loaded) return()
                   nPMds <- sum(Stochkeep$selected)
                   nSNs <- sum(SNkeep$selected)
                   plot_output_list <- lapply(1:nPMds, function(mm) {
                     row_name <- paste("PM", mm, sep="")
                     uiOutput(session$ns(row_name))

                   })
                   TagList <- do.call(tagList, plot_output_list)
                   ui <- tagList(
                     fluidRow(
                       column(2,
                              h3('Performance Metrics', class='lah')
                       ),
                       column(10,
                              h3('Operating Models',style="text-align: center;")
                       )
                     )
                   )
                   TagList <- c(ui, TagList)

                 })


                 observe({
                   if(!Object$Loaded) return()
                   nSNs <- sum(SNkeep$selected) # number SN selected
                   nMPs <- sum(MPkeep$selected) # n MPs selected
                   nPMds <- sum(Stochkeep$selected) # n PM selected
                   Codes <- Object$obj$Perf$Stoch$Codes[Stochkeep$selected] # PM codes
                   Labels <- Object$obj$Perf$Stoch$Labels[Stochkeep$selected] # PM codes
                   for (i in 1:nPMds) {
                     local({
                       my_i <- i
                       row_name <- paste("PM", my_i, sep="")
                       output[[row_name]] <- renderUI({
                         for (j in 1:nSNs) {
                           local({
                             my_j <- j
                             plot_name <- paste("plot", my_i, my_j, sep="")
                             output[[plot_name]] <- renderPlot({
                               Values <- Stoch$mat[,SNkeep$selected,
                                                   MPkeep$selected, Stochkeep$selected, drop=FALSE]
                               maxY <- max(Values)
                               dd <- dim(Values)
                               if (my_j <= dd[2] & my_i <= dd[4] & !any(dd==0)) {
                                 Val <- Values[,my_j,,my_i, drop=FALSE]
                                 MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected]
                                 trade_plot_OM(Val, maxY, MPcols, sn=my_j)
                               }

                             }, width=100, height=150)

                           })
                         }

                         myList <- lapply(1:nSNs, function(mm) {
                           plot_name <- paste("plot", my_i, mm, sep="")
                           plotOutput(session$ns(plot_name), width=100, height=150)
                         })
                         myList$cellArgs <- list(
                           style = "
                             width: 100px;
                             height: 150px;
                             margin: 5px;
                           ")

                         tagList(
                           fluidRow(
                             column(2,
                                    h3(Codes[my_i]),
                                    p(Labels[my_i]),
                                    style='padding-bottom:50px;'
                             ),
                             column(10,
                                    do.call(flowLayout, myList),
                                    style='padding-bottom:50px;'
                             )

                           )
                         )


                       })
                     })
                   }
                 })

               }
  )
}


Boxplot_OMUI <- function(id, label="boxplotOM") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             div(class='page_title',
                 h3('Trade-off and Performance', id='title'),
                 htmlOutput(ns('checkloaded')),
                 htmlOutput(ns('subtitle'))
             )
      )
    ),
    fluidRow(
      column(width = 8,
             div(
               summaryUI(ns('page9'))
             )
      )
    ),
    fluidRow(
      column(width=3, class='page_reading',
             div(
               h4(strong("READING THIS CHART")),
               htmlOutput(ns('reading')),
               br(),
               img(src='img/page_9_reading_crop.JPG',width=160,height=125)
             )

      ),
      column(width = 9, class='left_border',
             uiOutput(ns('results')
                      )
      )
    )
  )
}


trade_plot_OM <- function(Val, maxY, MPcols, sn) {

  med.cex <- 2
  rng.lwd <- 2
  main.cex <- 1.2

  if (all(dim(Val)>0)) {
    med <- apply(Val,3, median)
    rng <- apply(Val,3, range)

    nMPs <- length(MPcols)

    par(mfrow=c(1, 1), mar=c(0,0,0,0), oma=c(0,1.5,1,0))
    if (maxY > 100) {
      maxY <- 10^(ceiling(log10(maxY)))
    } else {
      maxY <- ceiling(maxY)
    }
    plot(c(0,nMPs+1), c(0,maxY), type="n", xlab='', ylab='', axes=FALSE)
    polygon(c(-0.1, nMPs+1.01, nMPs+1.01, -0.1),
            c(-0.1, -0.1, maxY+0.1, maxY+.1),
            col='#ededed', border=NA, xpd=NA)
    at <- seq(0, maxY, by=0.25*maxY)
    abline(h=at, col='white')
    mtext(side=3, sn, font=2, cex=main.cex, col='#D6501C', xpd=NA)
    points(1:nMPs, med, col=MPcols, cex=med.cex, pch=16, xpd=NA)
    for (i in 1:nMPs) {
      lines(c(i,i), rng[, i], col=MPcols[i], lwd=rng.lwd)
    }

    if (sn == 1) {
      text(rep(0.4, 3), c(0, 0.5*maxY, maxY), c(0, 0.5*maxY, maxY),
           col='#6e6b6b', xpd=NA, pos=2)
    }
  }

}




page_9_summary <-  function(Stoch, MPkeep, Stochkeep, SNkeep, Object, input) {
  Codes <- Object$obj$Perf$Stoch$Codes[Stochkeep$selected] # PM codes

  nSNs <- sum(SNkeep$selected) # number SN selected
  nMPs <- sum(MPkeep$selected) # n MPs selected
  nPMds <- sum(Stochkeep$selected) # n PM selected

  MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names

  if (nMPs>0 & nPMds >1 & nSNs>0) {

    if (nMPs<=2) {
      return('Please select 2 or more MPs')
    } else {
      Values <- Stoch$mat[,SNkeep$selected, MPkeep$selected, Stochkeep$selected]

      if(!any(dim(Values)<1)) {

        med <- apply(Values,3:4, median)
        str <- NULL

        for (i in 1:ncol(med)) {
          if (all(med[,i]==mean(med[,i]))) {
            mps <- paste0('all equal (', mean(med[,i]),')')
          } else {
            mps <- paste0(MPnames[which(med[,i]==max(med[,i]))])
          }

          str[i] <- paste0('MP(s) with highest median value for ', Codes[i], ': ',
                           paste(mps, collapse=", "), ' (', round(max(med[,i]),2), ')')

        }

        return(paste0(str, collapse = '\n'))
      }
    }
  }

}

# trade_plot_OMs <- function(Stoch, MPkeep, Stochkeep, SNkeep, PM, SN) {
#   Codes <- obj$Perf$Stoch$Codes[Stochkeep$selected] # PM codes
#   Labels <- obj$Perf$Stoch$Labels[Stochkeep$selected] # PM codes
#
#   nSNs <- sum(SNkeep$selected) # number SN selected
#   nMPs <- sum(MPkeep$selected) # n MPs selected
#   nPMds <- sum(Stochkeep$selected) # n PM selected
#
#   MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
#   MPnames <- obj$MP$Codes[MPkeep$selected] # MP names
#
#   if (nMPs>0 & nPMds >1 & nSNs>0) {
#     Values <- Stoch$mat[,SNkeep$selected, MPkeep$selected, Stochkeep$selected]
#
#     if(!any(dim(Values)<1)) {
#       med.cex <- 3
#       text.cex <- 2
#       rng.lwd <- 3
#       main.cex <- 2
#
#       dd <- dim(Values)[4]
#
#       if (PM <= dd) {
#         Val <- Values[,,,PM]
#         med <- apply(Val,c(2,3), median)
#         rng <- apply(Val,c(2,3), range)
#         nrow <- ceiling((nMPs*nSNs)/MPs.per.row)
#         ncol <- ceiling(nSNs/nrow)
#         ind <- c(1:nSNs, rep(NA, 100))
#         ind <- ind[1:(nrow*ncol)]
#         mat <- matrix(ind, nrow=nrow, ncol=ncol, byrow=TRUE)
#
#         par(mfrow=c(nrow, ncol), mar=c(3,3,3,1), oma=c(1,5,0,0))
#         for (sn in 1:nSNs) {
#           maxY <- max(Val)
#           if (maxY > 10) {
#             maxY <- 10^(ceiling(log10(maxY)))
#           } else {
#             maxY <- ceiling(maxY)
#           }
#           plot(c(0,nMPs+1), c(0,maxY), type="n", xlab='', ylab='', axes=FALSE)
#           polygon(c(-0.1, nMPs+1.01, nMPs+1.01, -0.1),
#                   c(-0.1, -0.1, maxY+0.1, maxY+.1),
#                   col='#dedede', border=NA, xpd=NA)
#           mtext(side=3, sn, font=2, cex=main.cex, col='#D6501C', xpd=NA)
#           if (sn == 1) {
#             text(-nMPs*0.5, 0.9*maxY, Codes[PM], font=2, xpd=NA, cex=text.cex)
#           }
#           if(sn %in% mat[,1]) {
#             text(rep(0.25, 3), c(0, 0.5*maxY, maxY), c(0, 0.5*maxY, maxY),
#                  col='#8c8c8c', xpd=NA)
#           }
#           points(1:nMPs, med[sn,], col=MPcols, cex=med.cex, pch=16, xpd=NA)
#           for (i in 1:nMPs) {
#             lines(c(i,i), rng[,sn, i], col=MPcols[i], lwd=rng.lwd)
#           }
#         }
#       }
#     }
#   }
# }


# This is page 9 of the Plot_Finals.pdf

BoxplotServer <- function(id, Stoch, MPkeep, Stochkeep, SNkeep, Object, i18n) {
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
                         h3(i18n()$t('Boxplot'), id='title')
                     )
                   )
                 })

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
                     div(class='page_title',
                         p(str, id='subtitle')
                     )
                   )
                 })

                 summaryServer('page8', Stoch, MPkeep, Stochkeep, SNkeep, Object,
                               page_8_summary, minPMs = 2)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Stochkeep$selected)
                   n.OM <- sum(SNkeep$selected)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Labels[MPkeep$selected] # MP names

                   icon_text <- paste('<i class="fas fa-circle fa-sm" style="color:', MPcols, ';"></i>', MPnames, '<br/>')
                   icon_text <- paste(icon_text, collapse=" ")

                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     tagList(
                       p(strong('Key'), HTML('<br/>'),
                         'Management Procedure', HTML('<br/>'),
                         HTML(icon_text)),
                       p(
                         'This chart', strong('compares'),
                         'performance of ', n.MP, ' management procedures (MP) ',
                         'across ', n.OM, ' operating models.'),
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
                   plot_output_list <- lapply(1:nPMds, function(mm) {
                     plotname <- paste("plot", mm, sep="")
                     shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width=250))
                   })
                   do.call(flowLayout , plot_output_list)
                 })

                 observe({
                   nPMds <- sum(Stochkeep$selected)
                   for (i in 1:nPMds) {
                     local({
                       my_i <- i
                       plotname <- paste("plot", my_i, sep="")
                       output[[plotname]] <- renderPlot({
                         trade_plot3(Stoch, MPkeep, Stochkeep, SNkeep, PM=my_i, Object$obj)
                       })
                     })
                   }
                 })



               }
  )
}


BoxplotUI <- function(id, label="NTP8") {
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
                       column(width = 8,
                              div(
                                summaryUI(ns('page8'))
                              )
                       )
                     ),
                     fluidRow(
                       column(width=3, class='page_reading',
                              div(
                                h4(strong("READING THIS CHART")),
                                htmlOutput(ns('reading')),
                                br(),
                                img(src='img/Boxplot.jpg', width='100%')
                              )

                       ),
                       column(width = 9, class='left_border',
                              uiOutput(ns('results'),
                                       height='650px')
                       )
                     )
    )
  )
}

trade_plot3 <- function(Stoch, MPkeep, Stochkeep, SNkeep, PM, obj) {
  Codes <- obj$Perf$Stoch$Codes[Stochkeep$selected] # PM codes

  nSNs <- sum(SNkeep$selected) # number SN selected
  nMPs <- sum(MPkeep$selected) # n MPs selected
  nPMds <- sum(Stochkeep$selected) # n PM selected

  MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Labels[MPkeep$selected] # MP names

  if (nMPs>1 & nPMds >1 & nSNs>0) {

    Values <- Stoch$mat[,SNkeep$selected, MPkeep$selected, Stochkeep$selected]

    if(!any(dim(Values)<1)) {
      med.cex <- 2.5
      qrt.lwd <- 4
      rng.lwd <- 1
      main.cex <- 1.25

      dd <- dim(Values)[4]

      if (PM <= dd) {
        Val <- Values[,,,PM]

        if (length(Val)>0) {
          med <- apply(Val,3, median, na.rm=TRUE)
          qrt <- apply(Val,3, quantile, c(0.25, 0.75), na.rm=TRUE)
          rng <<- apply(Val,3, range, na.rm=TRUE)

          maxY <- max(rng[is.finite(rng)])
          if (maxY > 10 & maxY<=100) {
            maxY <- 10^(ceiling(log10(maxY)))
          } else if (maxY <10) {
            maxY <- ceiling(maxY)
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
          points(1:nMPs, med, pch=16, col=MPcols, cex=med.cex, xpd=NA)
          for (i in 1:nMPs) {
            lines(c(i,i), qrt[,i], col=MPcols[i], lwd=qrt.lwd)
            lines(c(i,i), rng[,i], col=MPcols[i], lwd=rng.lwd)
          }
          mtext(side=3, line=0, Codes[PM], font=2, cex=main.cex,
                adj=0)
        }

      }

    }
  }


}


page_8_summary <-  function(Stoch, MPkeep, Stochkeep, SNkeep, Object, input) {
  Codes <- Object$obj$Perf$Stoch$Codes[Stochkeep$selected] # PM codes

  nSNs <- sum(SNkeep$selected) # number SN selected
  nMPs <- sum(MPkeep$selected) # n MPs selected
  nPMds <- sum(Stochkeep$selected) # n PM selected

  MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- Object$obj$MP$Labels[MPkeep$selected] # MP names

  if (nMPs>0 & nPMds >1 & nSNs>0) {


    if (nMPs<=1) {
      return('Please select 2 or more MPs')
    } else {
      Values <- Stoch$mat[,SNkeep$selected, MPkeep$selected, Stochkeep$selected]

      if(!any(dim(Values)<1)) {

        med <- apply(Values,3:4, median, na.rm=TRUE)
        str <- NULL

        for (i in 1:ncol(med)) {
          if (all(med[,i]==mean(med[,i], na.rm=TRUE))) {
            mps <- paste0('all equal (', mean(med[,i], na.rm=TRUE),')')
          } else {
            mps <- paste0(MPnames[which(med[,i]==max(med[,i], na.rm=TRUE))])
          }

          str[i] <- paste0('MP(s) with highest median value for ', Codes[i], ': ',
                           paste(mps, collapse=", "), ' (', round(max(med[,i], na.rm=TRUE),2), ')')

        }


        #LowestPM2 <- paste('MP with lowest value for', paste0(Codes[pm2], ":"),
        #                  paste(val, collapse=", "))

        return(paste0(str, collapse = '\n'))
    }




    }
  }

}

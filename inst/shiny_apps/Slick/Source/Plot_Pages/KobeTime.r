
# This is page 5 of the Plot_Finals.pdf

KobeTimeServer <- function(id, Proj, MPkeep, Projkeep, SNkeep, Object, i18n) {
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
                         h3(i18n()$t('Kobe Time'), id='title')
                     )
                   )
                 })

                 # subtitle - dynamic nMP and nOM
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
                     div(class='page_title',
                         p(str, id='subtitle')
                     )
                   )
                 })

                 summaryServer('page4', Proj, MPkeep, Projkeep, SNkeep, Object,
                                page_6_summary, minPMs=3, input)

                 output$reading <- renderUI({
                   if(!Object$Ready) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Projkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     tagList(
                       p(
                         'This chart', strong('compares'), 'and',
                         strong('ranks projected median values for ', n.OM,
                         'operating models over time for ', n.MP, 'management procedures'),
                         'and shows the levels of ', strong('uncertainty.'),
                         ' Segments within each bar are another way of looking',
                         'at the error bars in the Kobe plot. They show the',
                         'percentage of runs that fall in each of the Kobe',
                         'quadrants in each projection year. The probability of ',
                         'being in the green quadrant should be ', HTML(paste0(strong('>60%'),",")),
                         'a common management objective.'),
                       p('If specified, reference points are shown as dashed horizontal and/or vertical lines.'),
                       p('Select two trade-off Performance Metrics from the drop down menu on the right side.')
                     )
                   }
                 })

                 # output$explanation <- renderPlot({
                 #   plot(c(0,1))
                 # })

                 output$PM_dropdown <- renderUI({
                   if(!Object$Loaded) return()

                   Codes <- Object$obj$Perf$Proj$Codes[Projkeep$selected] # PM codes

                   sel1 <- Codes[grepl('BMSY', Codes)][1][[1]]
                   sel2 <- Codes[grepl('FMSY', Codes)][1][[1]]
                   if (is.na(sel1)|| is.null(sel1)) sel1 <- Codes[1][[1]]
                   if (is.na(sel2) || is.null(sel2)) sel2 <- Codes[2][[1]]

                   tagList(
                     h4('Select Performance Metrics'),
                     selectInput(session$ns('selectPM1'),
                                 'PM 1',
                                 choices=Codes,
                                 selected=sel1),
                     selectInput(session$ns('selectPM2'),
                                 'PM 2',
                                 choices=Codes,
                                 selected=sel2
                                 )
                   )
                 })

                 output$results <- renderUI({
                   if(!Object$Loaded) return()
                   nMPs <- sum(MPkeep$selected)
                       plot_output_list <- lapply(1:nMPs, function(mm) {
                         plotname <- paste("plot", mm, sep="")
                         shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width=250))

                       })

                   do.call(flowLayout , plot_output_list)
                   })

                 # Call renderPlot for each one. Plots are only actually generated when they
                 # are visible on the web page.
                 observe({
                   nMPs <- sum(MPkeep$selected)
                   for (i in 1:nMPs) {
                     # Need local so that each item gets its own number. Without it, the value
                     # of i in the renderPlot() will be the same across all instances, because
                     # of when the expression is evaluated.
                     local({
                       my_i <- i
                       plotname <- paste("plot", my_i, sep="")
                       output[[plotname]] <- renderPlot({
                         kobe_plot(Proj, MPkeep, Projkeep, SNkeep, input, mm=my_i, Object$obj)
                       })
                     })
                   }
                 })

                 output$perftab <- DT::renderDataTable({
                   if(!Object$Loaded) return()

                   nSN <- nrow(Object$obj$OM$Design) # number of SN in obj
                   nSNs <- sum(SNkeep$selected) # number SN selected

                   dd <- dim(Object$obj$Perf$Proj$Values)
                   n.yrs <- dd[5]
                   nPMd <- dd[4] # number PM in obj
                   nPMds <- sum(Projkeep$selected) # n PM selected

                   nMP <- length(Object$obj$MP$Labels) # n MPs in obj
                   nMPs <- sum(MPkeep$selected) # n MPs selected

                   cols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Labels[MPkeep$selected] # MP names

                   Codes <- Object$obj$Perf$Proj$Codes # PM codes

                   pm1 <- which(Codes==input$selectPM1)
                   pm2 <- which(Codes==input$selectPM2)


                   if (nMPs>0 & nPMds >1 & nSNs>0 & length(pm1>0) & length(pm2)>0) {
                     Values <- Proj$mat[,SNkeep$selected, MPkeep$selected,c(pm1, pm2), ,drop=FALSE] #

                     x_ref_names <- Object$obj$Perf$Proj$RefNames[[pm1]]
                     ind1 <- which(x_ref_names == "Target")
                     if (length(ind1)<1) ind1 <- 1
                     x_refs <- Object$obj$Perf$Proj$RefPoints[[pm1]]

                     y_ref_names <- Object$obj$Perf$Proj$RefNames[[pm2]]
                     ind2 <- which(y_ref_names == "Target")
                     if (length(ind2)<1) ind2 <- 1
                     y_refs <- Object$obj$Perf$Proj$RefPoints[[pm2]]

                     # All years
                     Green <- Values[,,,1,] > x_refs[ind1] & Values[,,,2,] < y_refs[ind2]
                     Red <- Values[,,,1,] < x_refs[ind1] & Values[,,,2,] > y_refs[ind2]
                     Yellow <- Values[,,,1,] > x_refs[ind1] & Values[,,,2,] > y_refs[ind2]
                     Orange <- Values[,,,1,] < x_refs[ind1] & Values[,,,2,] < y_refs[ind2]

                     meanGreen <- apply(Green, 3, mean) %>% round(2) * 100 %>% round()
                     meanOrange <- apply(Orange, 3, mean) %>% round(2) * 100 %>% round()
                     meanYellow <- apply(Yellow, 3, mean) %>% round(2) * 100 %>% round()
                     meanRed <- apply(Red, 3, mean) %>% round(2) * 100 %>% round()

                     # Terminal year
                     nyears <- dim(Values)[5]
                     Green <- Values[,,,1,nyears] > 1 & Values[,,,2,nyears] < 1
                     Red <- Values[,,,1,nyears] < 1 & Values[,,,2,nyears] > 1
                     Yellow <- Values[,,,1,nyears] > 1 & Values[,,,2,nyears] > 1
                     Orange <- Values[,,,1,nyears] < 1 & Values[,,,2,nyears] <1

                     meanGreen_y <- apply(Green, 3, mean) %>% round(2) * 100 %>% round()
                     meanOrange_y <- apply(Orange, 3, mean) %>% round(2) * 100 %>% round()
                     meanYellow_y <- apply(Yellow, 3, mean) %>% round(2) * 100 %>% round()
                     meanRed_y <- apply(Red, 3, mean) %>% round(2) * 100 %>% round()

                     DF <- data.frame(MP=MPnames,
                                      Green=round(meanGreen,0),
                                      Orange=round(meanOrange,0),
                                      Yellow=round(meanYellow,0),
                                      Red=round(meanRed,0),
                                      Green=round(meanGreen_y,0),
                                      Orange=round(meanOrange_y,0),
                                      Yellow=round(meanYellow_y,0),
                                      Red=round(meanRed_y,0))

                     # a custom table container
                     sketch = htmltools::withTags(table(
                       class = 'display',
                       thead(
                         tr(
                           th(colspan = 1, ''),
                           th(colspan = 4, 'All Years (%)', class = 'dt-center'),
                           th(colspan = 4, 'Terminal Year (%)', class = 'dt-center')
                         ),
                         tr(
                           th(colspan = 1, 'Management Procedure'),
                           lapply(c("Green", "Orange", 'Yellow', "Red"), th),
                           lapply(c("Green", "Orange", 'Yellow', "Red"), th)
                         )
                       )
                     ))

                     DT::datatable(DF, extensions = c('Responsive', 'Buttons'),
                                   container = sketch,
                                   options = list(
                                     paging = TRUE,
                                     searching = TRUE,
                                     fixedColumns = TRUE,
                                     autoWidth = FALSE,
                                     pageLength=25,
                                     ordering = TRUE,
                                     dom = 'Brtip',
                                     buttons = c('copy', 'csv', 'excel')
                                   ),
                                   rownames = FALSE)
                   }

                 })


               })
}


KobeTimeUI <- function(id, label="kobetime") {
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
                       column(width=5,
                              div(
                                summaryUI(ns('page4'))
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 6, class='page_reading',
                              h4(strong("READING THIS CHART")),
                              htmlOutput(ns('reading'))
                       ),
                       column(width=3,
                              br(),
                              br(),

                              img(src='img/KobeTime_corrected.jpg',
                                  style="width: 100%"
                              )
                       ),
                       column(width=1,
                       ),
                       column(width=2,
                              htmlOutput(ns('PM_dropdown'))
                       )
                     ),
                     fluidRow(
                       column(width=12, class='top_border',
                              uiOutput(ns('results'),
                                       # hover = ns("plot_hover"),
                                       height='650px'),
                              div(class="top_border",
                                  column(8,
                                         h3('Performance Table', class='lah'),
                                         DT::dataTableOutput(ns('perftab'))
                                  )
                              )
                       )
                     )
    )
  )
}



kobe_plot <- function(Proj, MPkeep, Projkeep, SNkeep, input, mm, obj) {

  nSN <- nrow(obj$OM$Design) # number of SN in obj
  nSNs <- sum(SNkeep$selected) # number SN selected

  dd <- dim(obj$Perf$Proj$Values)
  n.yrs <- dd[5]
  nPMd <- dd[4] # number PM in obj
  nPMds <- sum(Projkeep$selected) # n PM selected

  nMP <- length(obj$MP$Labels) # n MPs in obj
  nMPs <- sum(MPkeep$selected) # n MPs selected

  cols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Labels[MPkeep$selected] # MP names

  Codes <- obj$Perf$Proj$Codes # PM codes

  if (mm ==1) {
    incaxis <- TRUE
  } else {
    incaxis <- FALSE
  }
  incbox <- FALSE

  pm1 <- which(Codes==input$selectPM1)
  pm2 <- which(Codes==input$selectPM2)


  if (nMPs>0 & nPMds >1 & nSNs>0 & length(pm1>0) & length(pm2)>0) {
    Values <- Proj$mat[,SNkeep$selected, MPkeep$selected,c(pm1, pm2), ,drop=FALSE] #

    # make relative to ref points
    refpt1 <- obj$Perf$Proj$RefPoints[[pm1]]
    refnm1 <- obj$Perf$Proj$RefNames[[pm1]]
    refpt2 <- obj$Perf$Proj$RefPoints[[pm2]]
    refnm2 <- obj$Perf$Proj$RefNames[[pm2]]

    maxX <- 2 * max(refpt1) #quantile(Values[,,,1,],0.95)# max(Values[,,,1,]) # maximum of 2
    maxY <-  2 * max(refpt2) #quantile(Values[,,,2,],0.95)#max(Values[,,,2,])

    median1 <- apply(Values[,,,1,n.yrs, drop=FALSE], 3, median)
    median1[median1>maxX] <- maxX
    median2 <- apply(Values[,,,2,n.yrs, drop=FALSE], 3, median)
    median2[median2>maxY] <- maxY

    par(oma=c(1,1,1,1), mar=c(4,5,1,3), xaxs='i', yaxs='i')
    dd <- dim(Values)[3]

    if (mm <= dd) { # prevent plot when mm hasn't been updated yet
      quant1 <- quantile(Values[,,mm,1,n.yrs, drop=FALSE], c(0.05, 0.95), na.rm=TRUE)
      quant2 <- quantile(Values[,,mm,2,n.yrs, drop=FALSE], c(0.05, 0.95), na.rm=TRUE)

      small.mat <- matrix(c(0,1, 0, 0,rep(2,8)), nrow=6, ncol=2, byrow=TRUE)
      layout(small.mat, heights = c(3, 0.3, 1), widths = c(1, 3))

      layout.show(2)

      Kobe(median1, median2, quant1, quant2, input, maxX, maxY, refpt1, refpt2,
           refnm1, refnm2, mm, incaxis=incaxis, obj)
      Kobe_proj(Values, MPnames, mm, n.yrs, incbox=incbox, obj,
                refpt1, refpt2, refnm1, refnm2)


    }
  }
}


# Kobe plot
Kobe <- function(median1, median2, quant1, quant2, input, maxX, maxY, refpt1,
                 refpt2, refnm1, refnm2, mm, incaxis=FALSE, obj) {

  ind1 <- which(refnm1 =='Target')
  if (length(ind1)<1) ind1 <- 1
  ind2 <- which(refnm2 =='Target')
  if (length(ind2)<1) ind2 <- 1

  plot(c(0, maxX), c(0, maxY), xlab='', ylab='', axes=FALSE, bty="n", type="n")
  polygon(x=c(0, refpt1[ind1], refpt1[ind1], 0),
          y=c(0,0, refpt2[ind2], refpt2[ind2]), col=obj$Misc$Cols$KobeBG[2], border=NA)

  polygon(x=c(0, refpt1[ind1], refpt1[ind1], 0),
          y=c(refpt2[ind2],refpt2[ind2], maxY,maxY), col=obj$Misc$Cols$KobeBG[1], border=NA)

  polygon(x=c(refpt1[ind1], maxX, maxX, refpt1[ind1]),
          y=c(refpt2[ind2],refpt2[ind2], maxY,maxY), col=obj$Misc$Cols$KobeBG[3], border=NA)

  polygon(x=c(refpt1[ind1], maxX, maxX, refpt1[ind1]),
          y=c(0,0, refpt2[ind2], refpt2[ind2]), col=obj$Misc$Cols$KobeBG[4], border=NA)
  abline(h=refpt2, lty=3); abline(v=refpt1, lty=3)

  lines(quant1, rep(median2[mm],2), col="white", lwd=3)
  # lines(quant1, rep(median2[mm],2), col="darkgray", lwd=1)
  lines(rep(median1[mm],2), quant2, col="white", lwd=3)
  # lines(rep(median1[mm],2), quant2, col="darkgray", lwd=1)

  points(median1, median2, pch=16, col="#c7c7c7", cex=2, xpd=NA)
  points(median1[mm], median2[mm], pch=16, col="white", cex=2, xpd=NA)
  points(median1[mm], median2[mm], pch=16, col="black", cex=1.5, xpd=NA)

  # if (incaxis) {
    mtext(side=1, input$selectPM1, line=3)
    axis(side=1, at=c(0, refpt1, maxX))
    text(0, refpt2*1.1, refnm2, xpd=NA, pos=4, cex=1.5)
    mtext(side=2, input$selectPM2, line=3)
    axis(side=2, at=c(0, refpt2, maxY))
    text(refpt1*1.1, 0, refnm1, xpd=NA, pos=3, cex=1.5)
  # }
}

Kobe_proj <- function(Values, MPnames, mm, n.yrs, incbox=FALSE, obj,
                      refpt1, refpt2, refnm1, refnm2) {
  Years <- obj$Perf$Proj$Times
  xlab <- obj$Perf$Proj$Time_lab
  ylab <- 'Proportion of outcomes'
  yvals <- seq(0, 100, by=25)
  ylabs <- paste0(yvals, '%')

  lab.cex <- 1.2
  MPcex <- 2.2
  tex.cex <- 2
  ax.cex <- 1.5

  plot(range(Years), c(0, 100), axes=FALSE, type="n", xlab='', ylab='')
  axis(side=1, at=pretty(Years), xpd=NA, cex.axis=ax.cex)
  mtext(side=1, xlab, line=3,  cex=lab.cex)
  if (mm == 1) {
    axis(side=2, at=yvals, ylabs, las=1, xpd=NA, cex.axis=ax.cex, xpd=NA)
    mtext(side=2, ylab, line=4, cex=lab.cex)
  } else {
    axis(side=2, at=yvals, labels = FALSE)
  }

  ind1 <- which(refnm1 =='Target')
  if (length(ind1)<1) ind1 <- 1
  ind2 <- which(refnm2 =='Target')
  if (length(ind2)<1) ind2 <- 1


  fracBL <- fracBR <- fracTL <- fracTR <- rep(NA, length(Years))
  for (yr in seq_along(Years)) {
    n <- prod(dim(Values[,,mm,1,yr]))
    fracBR[yr] <- sum(Values[,,mm,1,yr] > refpt1[ind1] & Values[,,mm,2,yr] < refpt2[ind2])/n * 100
    fracBL[yr] <- sum(Values[,,mm,1,yr] < refpt1[ind1] & Values[,,mm,2,yr] < refpt2[ind2])/n * 100
    fracTL[yr] <- sum(Values[,,mm,1,yr] < refpt1[ind1] & Values[,,mm,2,yr] > refpt2[ind2])/n * 100
    fracTR[yr] <- sum(Values[,,mm,1,yr] > refpt1[ind1] & Values[,,mm,2,yr] > refpt2[ind2])/n * 100
  }

  polygon(x=c(Years, rev(Years)),
          y=c(rep(0, length(Years)), rev(fracTL)),
          col=obj$Misc$Cols$KobeBG[1],
          border=NA)

  polygon(x=c(Years, rev(Years)),
          y=c(fracTL, rev(fracTL+fracBL)),
          col=obj$Misc$Cols$KobeBG[2],
          border=NA)

  polygon(x=c(Years, rev(Years)),
          y=c(fracTL+fracBL, rev(fracTL+fracBL+fracTR)),
          col=obj$Misc$Cols$KobeBG[3],
          border=NA)

  polygon(x=c(Years, rev(Years)),
          y=c(fracTL+fracBL+fracTR,  rep(100, length(Years))),
          col=obj$Misc$Cols$KobeBG[4],
          border=NA)



  for (x in 2:length(yvals))
    lines(x=range(Years), y=rep(yvals[x], 2), col= grDevices::adjustcolor( "white", alpha.f = 0.5))

  # if (incbox) {
  #   polygon(x=c(Years[n.yrs-1], Years[n.yrs], Years[n.yrs], Years[n.yrs]-1),
  #           y=c(0, 0, 100, 100),
  #           col=NA, border='black', xpd=NA)
  #   text(Years[n.yrs-1], fracBR[n.yrs]*0.5,
  #        paste0(round(fracBR[n.yrs],2), "%"),
  #        pos=2, cex=tex.cex, xpd=NA)
  #   text(Years[n.yrs-1], 100-fracBR[n.yrs]+fracTR[n.yrs]*0.5,
  #        paste0(round(fracTR[n.yrs],2), "%"),
  #        pos=2, cex=tex.cex, xpd=NA)
  #   text(Years[n.yrs-1], fracBR[n.yrs]+fracTR[n.yrs]+fracBL[n.yrs]*0.5,
  #        paste0(round(fracBL[n.yrs],2), "%"),
  #        pos=2, cex=tex.cex, xpd=NA)
  #   text(Years[n.yrs-1], fracBR[n.yrs]+fracTR[n.yrs]+fracBL[n.yrs]+fracTL[n.yrs]*0.5,
  #        paste0(round(fracTL[n.yrs],2), "%"),
  #        pos=2, cex=tex.cex, xpd=NA)
  # }
  text(Years[1], 105, MPnames[mm], xpd=NA, font=2, pos=4, cex=MPcex)

}

# This is page 7 of the Plot_Finals.pdf

KobeServer <- function(id, Proj, MPkeep, Projkeep, SNkeep, Object) {
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

                 summaryServer('page6', Proj, MPkeep, Projkeep, SNkeep, Object,
                               page_6_summary, minPMs=2, input)

                 output$Select_quantiles <- renderUI({
                   tagList(
                     h4('Select Percentiles'),
                     sliderInput(session$ns('selectquant'),
                                 'Percentile',
                                 0,
                                 1,
                                 0.9,
                                 step=0.05
                     )
                   )
                 })
                 var <- reactiveValues(quant=0.9)
                 var$quant <- reactive({

                   input$selectquant
                 })

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Projkeep$selected)
                   n.OM <- sum(SNkeep$selected)

                   quant_text <- var$quant() * 100

                   yrs <- Object$obj$Perf$Proj$Times
                   yr.txt <- paste0(min(yrs), '-', max(yrs))
                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     tagList(
                       p(
                         'This chart', strong('compares trade-offs'), 'in',
                         n.MP, ' ', paste0(Object$obj$Misc$App_axes[3], 's'), 'for ', n.OM, ' ',
                         paste0(Object$obj$Misc$App_axes[2], 's'),
                         'by measuring two co-dependent performance metrics. These performance metrics can be modified under "Select Performance Metrics" above the plot.'),
                       p(
                         HTML('<i class="fas fa-circle fa-sm"></i>'),
                         'The', strong('dots'),
                         paste0('represent the median value for the final year of the projection period ',
                         yr.txt, '.')),
                       p(strong('Dotted lines'), 'around dots are error bars. The default represents',
                         HTML(paste0(quant_text, 'th')), 'percentiles, but that can be changed using the "Select Percentiles" scale at the right.')
                     )
                   }
                 })

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
                                 'Horizontal axis',
                                 choices=Codes,
                                 selected=sel1),
                     selectInput(session$ns('selectPM2'),
                                 'Vertical axis',
                                 choices=Codes,
                                 selected=sel2
                     )
                   )
                 })



                 output$trade_plot <- renderPlot({
                   if(!Object$Loaded) return()
                   trade_plot( Proj, MPkeep, Projkeep, SNkeep, input, Object$obj,
                               quant=var$quant())
                 })

               })
}


KobeUI <- function(id, label="kobe") {
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
               summaryUI(ns('page6'))
             )
      ),
      column(width = 4,
             htmlOutput(ns('PM_dropdown'))
             ),
      column(width = 2,
             htmlOutput(ns('Select_quantiles'))
      )
    ),
    fluidRow(
      column(width=3, class='page_reading',
             h4(strong("READING THIS CHART")),
             htmlOutput(ns('reading'))
      ),
      column(width=7,
             plotOutput(ns('trade_plot'))
      )
    )
  )
}


trade_plot <- function(Proj, MPkeep, Projkeep, SNkeep, input, obj, quant) {

  Codes <- obj$Perf$Proj$Codes # PM codes
  pm1 <- which(Codes==input$selectPM1)
  pm2 <- which(Codes==input$selectPM2)

  # for debugging
  if (length(pm1)<1) pm1 <-1
  if (length(pm2)<1) pm2 <-2

  nSNs <- sum(SNkeep$selected) # number SN selected
  nMPs <- sum(MPkeep$selected) # n MPs selected
  nPMds <- sum(Projkeep$selected) # n PM selected

  dd <- dim(obj$Perf$Proj$Values)
  n.yrs <- dd[5]

  # MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPcols <- rep('black', sum(MPkeep$selected)) # MP colors
  MPnames <- obj$MP$Codes[MPkeep$selected] # MP names

  med.cex <- 1.25

  quants <- c((1- quant)/2, 1-(1- quant)/2)

  if (nMPs>0 & nPMds >1 & nSNs>0) {
    Values <- Proj$mat[,SNkeep$selected, MPkeep$selected,c(pm1, pm2), ,drop=FALSE]

    if(!any(dim(Values)<1)) {
      med1 <- apply(Values[,,,1,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)
      med2 <- apply(Values[,,,2,n.yrs, drop=FALSE], 3, median, na.rm=TRUE)
      quant1 <- apply(Values[,,,1,n.yrs, drop=FALSE],  3,quantile, quants, na.rm=TRUE)
      quant2 <-apply(Values[,,,2,n.yrs, drop=FALSE],  3,quantile, quants, na.rm=TRUE)

      if (!is.null(dim(quant1)) & !is.null(dim(quant2))) {

        minX <- minY <- 0
        maxVal <- max(c(quant1, quant2))

        maxX <- max(quant1) # maxVal # max(quant1)
        maxY <- max(quant2) # maxVal #max(quant2)

        DF <- data.frame(x=med1, y=med2, MP=MPnames)

        # DF$MP <- factor(DF$MP, levels=MPnames[order(abs(DF$x-1) + abs(DF$y-1))],
        # ordered=TRUE)
        DF$MP <- factor(DF$MP, levels=MPnames,
                        ordered=TRUE)

        XError <- data.frame(x=c(quant1[1,], quant1[2,]),
                             y=rep(med2,2),
                             MP=rep(MPnames,2))
        YError <- data.frame(x=rep(med1,2),
                             y=c(quant2[1,], quant2[2,]),
                             MP=rep(MPnames,2))

        # Ref Points
        x_ref_names <- obj$Perf$Proj$RefNames[[pm1]]
        ind1 <- which(x_ref_names == "Target")
        if (length(ind1)<1) ind1 <- 1


        x_refs <- obj$Perf$Proj$RefPoints[[pm1]]
        y_ref_names <- obj$Perf$Proj$RefNames[[pm2]]
        ind2 <- which(y_ref_names == "Target")
        if (length(ind2)<1) ind2 <- 1
        y_refs <- obj$Perf$Proj$RefPoints[[pm2]]

        # x_refs <- x_refs2 <- rep(x_refs,each=2)
        # y_refs <- y_refs2 <- rep(y_refs,each=2)
        # y_refs2[c(1,3)] <- 0
        # x_refs2[c(1,3)] <- 0
        # XRef_DF <- data.frame(x=x_refs,
        #                       y=y_refs2,
        #                       name=rep(x_ref_names,each=2))
        # YRef_DF <- data.frame(x=x_refs2,
        #                       y=y_refs,
        #                       name=rep(y_ref_names,each=2))


        BL <- data.frame(x=c(0,x_refs[ind1],x_refs[ind1],0),
                         y=c(0,0,y_refs[ind2],y_refs[ind2]), lab="BL")
        TL <- data.frame(x=c(0,x_refs[ind1],x_refs[ind1],0),
                         y=c(y_refs[ind2],y_refs[ind2],maxY,maxY), lab="TL")
        TR <- data.frame(x=c(x_refs[ind1], maxX, maxX, x_refs[ind1]),
                         y=c(y_refs[ind2],y_refs[ind2], maxY,maxY), lab="TR")
        BR <- data.frame(x=c(x_refs[ind1], maxX, maxX, x_refs[ind1]),
                         y=c(0,0, y_refs[ind2], y_refs[ind2]), lab="BR")
        bgDF <- rbind(BL, TL, TR, BR)
        bgCols <- c(BL=as.character(obj$Misc$Cols$KobeBG[3]),
                    TL=as.character(obj$Misc$Cols$KobeBG[1]),
                    TR=as.character(obj$Misc$Cols$KobeBG[2]),
                    BR=as.character(obj$Misc$Cols$KobeBG[4]))



        ggplot() +
          geom_polygon(data=bgDF, aes(x=x, y=y, fill=lab), alpha=0.5) +
          scale_fill_manual(values=bgCols) +
          # geom_line(data=XRef_DF, aes(x=x, y=y, linetype=name),
          #           color=c('red', 'red', 'green', 'green'), size=1.2) +
          # geom_line(data=YRef_DF, aes(x=x, y=y, linetype=name),
          #           color=c('red', 'red', 'green', 'green'), size=1.2) +
          geom_line(data=XError, aes(x=x, y=y, group=MP),
                    color='white', linetype='dotted', size=1) +
          geom_line(data=YError, aes(x=x, y=y, group=MP),
                    color='white', linetype='dotted', size=1) +
          geom_point(data=DF, aes(x=x, y=y), color='white', size=4) +
          geom_point(data=DF, aes(x=x, y=y, color=MP), size=4) +
          ggrepel::geom_text_repel(data=DF, aes(x=x, y=y, color=MP, label=MP),
                                   size=6, fontface ='bold',
                                   show.legend = FALSE) +
          scale_color_manual(values=MPcols) +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_linetype_manual(values = c(2,3)) +
          theme_classic() +
          coord_cartesian(clip = 'off') +
          guides(fill=FALSE, label=FALSE,
                 linetype=guide_legend(keywidth = 3, keyheight = 1)) +
          labs(x=input$selectPM1,
               y=input$selectPM2,
               color=obj$Misc$App_axes[3],
               linetype="Reference Points") +
          theme(
            text = element_text(size=20)
          )+
          theme(legend.position = "none")
      }

    }
  }
}


page_6_summary <-  function(Proj, MPkeep, Projkeep, SNkeep, Object, input) {

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

    x_ref_names <- obj$Perf$Proj$RefNames[[pm1]]
    ind1 <- which(x_ref_names == "Target")
    if (length(ind1)<1) ind1 <- 1
    x_refs <- obj$Perf$Proj$RefPoints[[pm1]]

    y_ref_names <- obj$Perf$Proj$RefNames[[pm2]]
    ind2 <- which(y_ref_names == "Target")
    if (length(ind2)<1) ind2 <- 1
    y_refs <- obj$Perf$Proj$RefPoints[[pm2]]

    Values <- Proj$mat[,SNkeep$selected, MPkeep$selected,c(pm1, pm2), ,drop=FALSE] #

    Green <- Values[,,,1,] > x_refs[ind1] & Values[,,,2,] < y_refs[ind2]
    Red <- Values[,,,1,] < x_refs[ind1] & Values[,,,2,] > y_refs[ind2]

    meanGreen <- apply(Green, 3, mean)
    if (any(meanGreen>0)) {
      GreenMPs <- MPnames[which.max(meanGreen)]
    } else {
      GreenMPs <- 'None'
    }
    meanRed <- apply(Red, 3, mean)
    if (any(meanRed>0)) {
      RedMPs <- MPnames[which.max(meanRed)]
    } else {
      RedMPs <- 'None'
    }

    Highest <- paste('MP with highest probability of being in green region:', paste(GreenMPs, collapse=", "))
    Lowest <- paste('MP with highest probability of being in red region:', paste(RedMPs, collapse=", "))

    return( paste0(c(Highest, Lowest), collapse = '\n'))
  }


}


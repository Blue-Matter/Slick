# ---- Page 10 - Performance Comparison ----

# This is page 11 of Plot_Finals.pdf

Spider_OMServer <- function(id, Det, MPkeep, Detkeep, SNkeep, Object) {
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

                 summaryServer('page10', Det, MPkeep, Detkeep, SNkeep, Object,
                               page_10_summary)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Detkeep$selected)
                   n.OM <- sum(SNkeep$selected)

                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     tagList(
                       p('This chart', strong('compares performance'), 'of',
                         n.PM, strong('performance metrics'), 'in', n.MP,
                         strong('management procedures (MP)'), 'for a set of',
                         n.OM, strong('operating models'), '(columns).'),
                       p(strong('The polygon edges in each chart'), 'connect',
                         strong('individual scores'), 'for the performance metrics in that management procedure.',
                         'Points closer to the exterior edge indicate better performance.'),
                       p('The', strong('percentages'), 'represent an ',
                         strong('average score of all'), 'performance metrics in each management procedure.',
                         'It provides a quick comparison of overall MP performances.',
                         strong('Filled hexagons with larger areas indicate better overall performance.')),
                       p('For each operating model, the management procedures are ordered from highest to lowest overall average score.'),
                       p('These summary values assume equal weighting and equal scaling of performance metrics.')
                     )
                   }
                 })

                 output$MPlist <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names
                   MPnames <- paste(" ", MPnames)

                   icon_text <- paste('<i class="fas fa-hexagon fa-sm" style="color:',
                                      MPcols, ';"></i>', '  ',MPnames, '<br/>')
                   icon_text <- paste(icon_text, collapse=" ")

                   if (n.MP>0) {
                     tagList(
                       p(
                         HTML(icon_text)
                       )
                     )
                   }
                 })

                 output$PMlist <- renderUI({
                   if(!Object$Loaded) return()
                   n.PM <- sum(Detkeep$selected)
                   PM.name <- Object$obj$Perf$Det$Labels
                   # PM.name <- Object$obj$Perf$Det$Codes
                   # PM.desc <- Object$obj$Perf$Det$Description
                   lets <- LETTERS[1:n.PM]


                   icon_shape <- paste('<span class="circle"">', lets, '</span>')

                   if (n.PM >2) {
                     text <- rep('', n.PM)
                     for (i in 1:n.PM) {
                       text[i] <- paste(icon_shape[i],
                                        "<span><strong>",
                                        PM.name[i],
                                        "</strong></span>")
                                        # PM.desc[i],
                                        # '</span>')
                     }
                     tagList(
                       p(
                         HTML( paste(text, collapse="<br>"))
                       , width='100%')
                     )

                   }
                 })

                 output$PM_outline <- renderPlot({
                   if(!Object$Loaded) return()
                   n.PM <- sum(Detkeep$selected)

                   if (n.PM>2) {
                     pm_outline_plot(n.PM)
                   }
                 }, width=125, height=125)


                 output$results <- renderUI({
                   if(!Object$Loaded) return()
                   n.SN <- sum(SNkeep$selected)
                   SN.select <- which(SNkeep$selected)
                   n.PM <- sum(Detkeep$selected)
                   nMPs <- sum(MPkeep$selected)
                   if (nMPs >=2) {

                     if (n.PM<3) {
                       tagList(
                         h3('Please select 3 or more Peformance Metrics')
                       )
                     } else {
                       hgt <- paste0(90 * nMPs, 'px')
                       plot_output_list <- lapply(SN.select, function(mm) {
                         plotname <- paste("plot", mm, sep="")

                         tagList(
                           h4(mm, class='OM_name'),
                           div(
                             plotOutput(session$ns(plotname), width='90px', height=hgt),
                             style="padding-right:50px;  background-color: #f5f5f5;;"

                           )

                         )

                       })
                       plot_output_list$cellArgs <- list(
                         style = "
                     width: 100px;

                     "
                       )
                       do.call(flowLayout , plot_output_list)
                     }

                   } else {
                     tagList(
                       h3('Please select 2 or more MPs')
                     )
                   }

                 })

                 observe({
                   if(!Object$Loaded) return()
                   n.SN <- sum(SNkeep$selected)
                   SN.select <- which(SNkeep$selected)

                   if(length(Det$mat)>1) {
                     OM.res <- Det$mat[, MPkeep$selected, Detkeep$selected, drop=FALSE]

                     MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected]

                     for (i in 1:length(SNkeep$selected)) {

                       if (i %in% SN.select) {
                         local({
                           my_i <- i
                           plotname <- paste("plot", my_i, sep="")
                           output[[plotname]] <- renderPlot({
                             hexplot_OM(OM.res[my_i,,], MPcols)
                           }, height=function(){
                             nMPs <- sum(MPkeep$selected)
                             90 * nMPs
                           })
                         })
                       }

                     }
                   }

                 })

               }


  )
}


Spider_OMUI <- function(id, label="spiderOM") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             div(class='page_title',
                 h3('Performance comparison', id='title'),
                 htmlOutput(ns('checkloaded')),
                 htmlOutput(ns('subtitle'))
             )
      )
    ),
    fluidRow(
      column(width = 5,
             div(
               summaryUI(ns('page10'))
             )
      )
    ),
    fluidRow(
      column(width=2, class='page_reading_rb',
             div(
               h4(strong("READING THIS CHART")),
               htmlOutput(ns('reading')),
               br(),
               img(src='img/page_10_reading.JPG')
             )

      ),
      column(width = 10,
             fluidRow(
              column(width=2,
                     h4('Management procedure'),
                     htmlOutput(ns('MPlist'))
                     ),
              column(width=10, class='left_border',
                     h4('Performance metrics measured'),
                     fluidRow(
                       column(2,
                              plotOutput(ns('PM_outline'), width=125, height=125),
                              ),
                       column(10,
                              htmlOutput(ns('PMlist'))
                              )
                     )
              )

             ),
             fluidRow(class='top_border',
               column(width=12,
                      h4('Operating Model'),
                      uiOutput(ns('results'), height='650px')
               )
             )
      )

    )
  )
}


pm_outline_plot <- function(n.PM) {
  par(mfrow=c(1,1), oma=c(1,1,1,1), mar=c(0,0,0,0))
  line.col <- 'darkgrey'
  pt.cex <- 3

  vertices <- polyCoords(n.PM) * 100
  plot(vertices, type="l", col=line.col, axes=FALSE, xlab="", ylab="", xpd=NA)
  lines(vertices*0.66, type="l", col=line.col, xpd=NA)
  lines(vertices*0.33, type="l", col=line.col, xpd=NA)
  for (i in 1:(nrow(vertices)-1)) {
    lines(x=c(0, vertices[i,1]),
          y=c(0, vertices[i,2]), col=line.col)
    points(x=vertices[i,1], y=vertices[i,2], pch=16, col=line.col, cex=pt.cex, xpd=NA)
    text(x=vertices[i,1], y=vertices[i,2], col='white', LETTERS[i], xpd=NA)
  }

}

hexplot_OM <- function(OM.res, MPcols) {
  dd <- dim(OM.res)
  n.PM <- dd[2]
  n.MP <- dd[1]

  fill.col <- '#cccccc'
  lwd <- 1
  mplab.cex <- 2.5
  pt.cex <- 2
  pt.col <- 'darkred'
  if (!is.null(n.PM)) {
    vertices <- polyCoords(n.PM) * 100

    meanVals <- apply(OM.res, 1, mean)
    MP.ord <- rev(order(meanVals))

    meanVals <- paste0(round(meanVals,0), '%')

    maxScore <- OM.res == 100

    par(mfrow=c(n.MP, 1), oma=c(0,0,0,0), mar=c(0,0,0,0), bg=NA)
    # loop over MPs
    if (all(is.finite(vertices))) {
      for (r in MP.ord) {
        plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
        polygon(vertices, col=fill.col, border=NA)

        coords <- NULL
        for (j in 1:n.PM) {
          pts <- calcCoord(vertices[j,], OM.res[r,j])
          coords <- rbind(coords, pts )
        }
        coords <- rbind(coords, coords[1,])
        polygon(coords, col=MPcols[r], border=NA)

        for (j in 1:n.PM) {
          if (maxScore[r,j])
            points(coords[j,], cex=pt.cex, col=pt.col, pch=16)
        }

        text(0,0, meanVals[r], col="black", cex=mplab.cex, font=2)
      }
    }
  }


}



page_10_summary <- function(Det, MPkeep, Detkeep, SNkeep, Object) {

  nSN <- nrow(Object$obj$OM$Design)
  nSNs <- sum(SNkeep$selected)

  nPMd <- dim(Object$obj$Perf$Det$Values)[3]
  nPMds <- sum(Detkeep$selected)

  nMP <- length(Object$obj$MP$Labels)
  nMPs <- sum(MPkeep$selected)

  # median PM values across OMs
  pm <- apply(Det$mat[SNkeep$selected,MPkeep$selected,Detkeep$selected,drop=FALSE], c(1,2), mean, na.rm=TRUE)

  if(!any(dim(pm)<1)) {

    if (nMPs<=1) return('Please select 2 or more MPs')

    MPnames <- Object$obj$MP$Labels[MPkeep$selected]
    str <- NULL

    selectedOMs <- (1:nSN)[SNkeep$selected]

    for (i in seq_along(selectedOMs)) {
      if (all(pm[i,]==mean(pm[i,]))) {
        mps <- paste0('all equal (', mean(pm[i,]),')')
      } else {
        mps <- paste0(MPnames[which(pm[i,]==max(pm[i,]))])
      }

      str[i] <- paste0(selectedOMs[i], ': ',
                       paste(mps, collapse=", "), ' (', round(max(pm[i,]),2), ')')

    }
    return(paste0('MP(s) with highest overall mean value for OM:\n',
                  paste0(str, collapse = '\n')))

  }
}








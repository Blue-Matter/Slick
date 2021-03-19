# ---- Page 1 - Performance Comparison ----

# This is page 1 and page 2 of Plot_Finals.pdf

SpiderServer <- function(id, Det, MPkeep, Detkeep, SNkeep, Object) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- CheckLoaded(Object)

                 # subtitle - dynamic nMP and nOM
                 output$subtitle <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   n.PM <- sum(Detkeep$selected)

                   if (n.PM <=2) {
                     return(
                       tagList(
                         p('Please select 3 or more Performance Metrics', id='subtitle',
                           style="color:red;")
                       )
                     )
                   }

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

                 summaryServer('page1', Det, MPkeep, Detkeep, SNkeep, Object,
                               page_1_summary)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Detkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     if (n.PM<3) {
                       tagList(p('Please select 3 or more Performance Metrics.'))
                     } else {
                       tagList(
                         p('This chart', strong('compares the performance of ', n.MP,
                                                ' management procedures (MP) against ', n.PM,
                                                ' performance metrics.')),
                         p('Each value is a median peformance metric over ', n.OM,
                           ' operating models.'),
                         p(HTML('<i class="fas fa-hexagon"></i>'),
                           'The', strong('filled symbols on top'),
                           'represent an average score of all performance metrics for',
                           'each management procedure. It provides a quick comparison',
                           'of overall MP performances.',
                           strong('Larger areas indicate better overall performance. '),
                           'These summary values assume equal weighting and equal scaling of performance metrics.'),

                         p(HTML('<i class="far fa-hexagon"></i>'),
                           strong('The lines in the bottom spider plot'),
                           'connect', strong('individual scores'),
                           'for the performance metrics in each management procedure.',
                           'Scores closer to the exterior edge indicate better performance.'),
                         p('The Relative Scale button rescales each performance metric shown in the plots between the maximum and minimum values of that metric. When Relative Scale is on, the center of the spider plot will represent the lowest value for each performance metric, and the outside edge of the spider plot will represent the highest value for each metric.'
                         )
                       )
                     }

                   }

                 })

                 output$mpsub <- renderUI({
                   if(!Object$Loaded) return()
                   n.select <- sum(Detkeep$selected)
                   if (n.select>2) {
                     tagList(
                       fluidRow(
                         column(width = 6,
                                h3('Management Procedure', class='lah'),
                                h4(strong('Overall scores'), '(average of ', n.select,
                                   'performance metrics).')
                       ),
                       column(width = 1),
                       column(width = 5,
                              h4('Relative Scale'),
                              switchInput(
                                inputId = session$ns("RS_button"),
                                handleWidth = 80, labelWidth = 40,
                                inline = TRUE, width = "200px"
                                )
                              )
                       )
                     )
                   }
                 })

                 SwitchScale <- reactiveValues(relative=FALSE)

                 observeEvent(input$RS_button, {
                   SwitchScale$relative <- input$RS_button
                 })

                 output$filled_hex <- renderPlot({
                   if(!Object$Loaded) return()
                   hexplot_fun(Det, MPkeep, Detkeep, SNkeep, Object, SwitchScale)
                 }, height=function(){
                   n.MP <- sum(MPkeep$selected)
                   if (n.MP<1) return(175)
                   n.row <- ceiling(n.MP/4)
                   175*n.row
                 }, width=function(){
                   n.MP <- sum(MPkeep$selected)
                   if (n.MP<1) return(175)
                   n.row <- ceiling(n.MP/4)
                   n.col <- ceiling(n.MP/n.row)
                   175*n.col
                 })

                 output$spider_plot <- renderPlot({
                   if(!Object$Loaded) return()

                   spiderplot_fun(Det, MPkeep, Detkeep, SNkeep, Object, SwitchScale)


                 }, width=function() {
                   dims <- window_dims()
                   dims[1]*0.2
                 }, height=function() {
                   dims <- window_dims()
                   dims[1]*0.2

                 }
                 )

                 output$perftab <- DT::renderDataTable({
                   if(!Object$Loaded) return()
                   # select PMs and MPs
                   pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)
                   pm <- round(pm, 2)
                   pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]
                   colnames(pm) <- Object$obj$Perf$Det$Codes[Detkeep$selected]
                   rownames(pm) <- Object$obj$MP$Labels[MPkeep$selected]

                   nPM <- dim(pm)[2]
                   # a custom table container
                   sketch = htmltools::withTags(table(
                     class = 'display',
                     thead(
                       tr(
                         th(rowspan = 2, 'Management Procedures'),
                         th(colspan = nPM, 'Performance Metrics')
                       ),
                       tr(
                         lapply(colnames(pm), th)
                       )
                     )
                   ))

                   DT::datatable(pm, extensions = c('Responsive', 'Buttons'),
                                 container = sketch,
                                 options = list(
                                   paging = TRUE,
                                   searching = TRUE,
                                   fixedColumns = TRUE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel')
                                 ))

                 })

               }
  )
}


SpiderUI <- function(id, label="spider") {

  ns <- NS(id)
  tagList(
    fluidRow(

      column(width = 6,
             div(class='page_title',
                 h3('Performance Comparison', id='title'),
                 htmlOutput(ns('checkloaded')),
                 htmlOutput(ns('subtitle'))
                 )
             )
    ),
    fluidRow(
      column(width=5,
             div(
               summaryUI(ns('page1'))
                 # uiOutput(ns('summary'), class='page_summary')
                 )
             )
    ),
    fluidRow(class='bottom_border',
          column(width = 3, class='right_border',
                 fluidRow(class='page_reading',
                          column(12,
                                 h4(strong("READING THIS CHART")),
                                 htmlOutput(ns('reading'))
                          )
                 )

          ),
          column(width=9,
                 div(class='filled_hex',
                     uiOutput(ns('mpsub'), class='lah'),
                     plotOutput(ns('filled_hex'), height='auto')
                 )
          )
    ),
    fluidRow(
      column(width=4, class='right_border',
             div(class="spider_plot", align='center',
                 h3('Performance Metrics', class='lah'),
                 plotOutput(ns('spider_plot'),height='600px')
             )
      ),
      column(width=8, class='perftable',
             h3('Performance Table', class='lah'),
             DT::dataTableOutput(ns('perftab'))
      )
    )
  )
}










polyCoords <- function(n){
  # https://stackoverflow.com/a/29172340/2885462
  sq<-2*pi*(0:n)/n
  cbind(sin(sq),cos(sq))
}


spiderplot_fun <- function(Det, MPkeep, Detkeep, SNkeep, Object, SwitchScale) {

  nSN <- nrow(Object$obj$OM$Design)
  nSNs <- sum(SNkeep$selected)

  nPMd <- dim(Object$obj$Perf$Det$Values)[3]
  nPMds <- sum(Detkeep$selected)

  nMP <- length(Object$obj$MP$Labels)
  nMPs <- sum(MPkeep$selected)


  # plot controls
  fill.col <- '#f2f3f5'
  line.col <- 'white'
  lwd <- 3

  # draw blank shape
  if(nPMds >2) {
    vertices <- polyCoords(nPMds) * 100
    par(mfrow=c(1,1), mar=c(3,3,3,3), oma=c(2,2,2,2))
    plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
    polygon(vertices, col=fill.col, border=NA)
    for (i in 1:nrow(vertices)) lines(c(0, vertices[i,1]), c(0, vertices[i,2]),
                                      col=line.col)

    # Calc median PM over OMs
    pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)
    pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]

    if (SwitchScale$relative) {
      # make all PMs relative to maximum and minimum values
      normalize <- function(x) {
        return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
      }
      if(nrow(pm)==1) {
        pm <- normalize(pm) * 100
      } else {
        pm <- apply(pm, 2, normalize) * 100
      }
    }

    # calculate coordinates for each MP
    # & plot lines for each mp

    cols <- Object$obj$Misc$Cols$MP[MPkeep$selected]
    Codes <- Object$obj$Perf$Det$Codes[Detkeep$selected]
    for (i in 1:nMPs) {
      coords <- NULL
      for (j in 1:nPMds) {
        pts <- calcCoord(vertices[j,], pm[i,j])
        coords <- rbind(coords, pts )
      }
      coords <- rbind(coords, coords[1,])
      lines(coords, col=cols[i], lwd=lwd)
    }

    for (j in 1:nPMds) {
      loc <- vertices[j,]
      loc <- round(loc,0) *1.05#* c(1.05, 0.95)
      if (loc[1]<0 & loc[2]<=0) pos <- 1
      if (loc[1]<0 & loc[2]>0) pos <- 2
      if (loc[1]>=0 & loc[2]>0) pos <- 3
      if (loc[1]>=0 & loc[2]<=0) pos <- 4

      txt <- Codes[j]
      text(loc[1],loc[2], pos=pos, txt, xpd=NA, col='black')

    }
  }

}


calcCoord <- function(vert, pm) {
  flipy <- flipx <- FALSE
  if (vert[1]< 0) {
    flipx <- TRUE
    vert[1] <- abs(vert[1])
  }

  if (vert[2] < 0) {
    flipy <- TRUE
    vert[2] <- abs(vert[2])
  }

  theta <- atan(vert[2]/vert[1])
  y <- sin(theta) * pm
  if (flipy) y <- -y
  x <- sqrt(pm^2 - y^2)
  if (flipx) x <- -x
  data.frame(x,y)
}


hexplot_fun <- function(Det, MPkeep, Detkeep, SNkeep, Object, SwitchScale) {

  nSN <- nrow(Object$obj$OM$Design)
  nSNs <- sum(SNkeep$selected)

  nPMd <- dim(Object$obj$Perf$Det$Values)[3]
  nPMds <- sum(Detkeep$selected)

  nMP <- length(Object$obj$MP$Labels)
  nMPs <- sum(MPkeep$selected)

  # plot controls
  fill.col <- '#f2f3f5'
  line.col <- 'white'
  lwd <- 3
  cols <- Object$obj$Misc$Cols$MP

  n.row <- ceiling(nMPs/4)
  n.col <- ceiling(nMPs/n.row)

  mplab.cex <- 2.2
  pm.avg.cex <- 2.5

  if (nMPs>0 & nPMds >2 & nSNs>0) {
    pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)

    if (SwitchScale$relative) {
      # make all PMs relative to maximum and minimum values
      normalize <- function(x) {
        return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
      }

      pm2 <- pm[MPkeep$selected,]
      if('numeric' %in% class(pm2)) {
        pm2 <- normalize(pm2) * 100
        pm[MPkeep$selected,] <- pm2
      } else {
        pm <- apply(pm, 2, normalize) * 100
      }
    }

    pm <- pm[,Detkeep$selected]
    pm.avg <- apply(pm, 1, mean, na.rm=TRUE)

    par(mfrow=c(n.row, n.col), mar=c(3,3,3,3), oma=rep(0,4))
    for (i in 1:nMP) {
      if (MPkeep$selected[i]) {

        # draw blank shape
        vertices <- polyCoords(nPMds) * 100
        plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
        polygon(vertices, col=fill.col, border=NA)

        coords <- NULL
        for (j in 1:nPMds) {
          pts <- calcCoord(vertices[j,], pm[i,j])
          coords <- rbind(coords, pts )
        }
        coords <- rbind(coords, coords[1,])
        polygon(coords, col=cols[i], border=cols[i])
        text(0,0, round(pm.avg[i], 2), col="black", cex=mplab.cex, font=2)
        text(0, 100, Object$obj$MP$Codes[i], xpd=NA, col=cols[i], cex=mplab.cex,
             pos=3)
      }
    }
  }

}


CheckLoaded <- function(Object) {
  renderUI({
    if(!Object$Loaded)
      h4('Slick object not loaded. Please return to ', a('Home', onclick='customHref("splash")',
                                        style='color:red; cursor: pointer;'),
         'and load a Slick object', style = "color:red")
  })
}


page_1_summary <- function(Det, MPkeep, Detkeep, SNkeep, Object) {

  nSN <- nrow(Object$obj$OM$Design)
  nSNs <- sum(SNkeep$selected)

  nPMd <- dim(Object$obj$Perf$Det$Values)[3]
  nPMds <- sum(Detkeep$selected)

  nMP <- length(Object$obj$MP$Labels)
  nMPs <- sum(MPkeep$selected)

  # median PM values across OMs
  pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)

  # Highest and lowest overall score
  pm <- pm[MPkeep$selected,Detkeep$selected, drop=FALSE]
  pm.avg <- apply(pm, 1, mean, na.rm=TRUE)
  # pm.avg <- pm.avg[MPkeep$selected]

  MPnames <- Object$obj$MP$Codes[MPkeep$selected]

  Highest <- paste('MP with highest average performance value:', paste(MPnames[which.max(pm.avg)], collapse=", "))
  Lowest <- paste('MP with lowest average performance value:', paste(MPnames[which.min(pm.avg)], collapse=", "))

  # Dominated MPs
  domText <- 'No MPs are outperformed by all the others.'

  DomMPs <- list()

  for (i in 1:nMPs) {
      mat <- matrix(pm[i,], nrow=nMPs, ncol=nPMds, byrow=TRUE)
      dom <- apply(mat < pm, 1, prod)
      if (any(dom>0)) {
        domMP <- paste( MPnames[dom>0], collapse=", ")
        DomMPs[[i]] <- paste(MPnames[i], 'dominated by:', domMP)
      }
  }
  if (any(unlist(lapply(DomMPs, length))>0)) {
    domText <- do.call('rbind', DomMPs)
    domText <- paste(domText, collapse = "\n")
  }

  paste0(c(Highest, Lowest, domText), collapse = '\n')
}


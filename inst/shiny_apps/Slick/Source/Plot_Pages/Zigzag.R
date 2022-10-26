# ---- Page 2 - Performance Comparison ----

# This is page 3 of the Plot_Finals.pdf

ZigzagServer <- function(id, Det, MPkeep, Detkeep, SNkeep, Object, window_dims, i18n) {
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
                         h3(i18n()$t('Zigzag'), id='title')
                     )
                   )
                 })

                 # subtitle - dynamic nMP and nOM
                 output$subtitle <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   n.PM <- sum(Detkeep$selected)
                   if (n.PM <=1) {
                     return(
                       tagList(
                         div(class='page_title',
                         p('Please select 3 or more Performance Metrics', id='subtitle',
                           style="color:red;")
                         )
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
                     div(class='page_title',
                     p(str, id='subtitle')
                     )
                   )
                 })

                 summaryServer('page2', Det, MPkeep, Detkeep, SNkeep, Object,
                               page_2_summary,minPMs=2)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Detkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   if (n.MP>0 & n.PM>0 & n.OM>0) {
                     if (n.PM<3) {
                       tagList(p('Please select 2 or more Performance Metrics.'))
                     } else {
                     tagList(
                       p('This chart',
                         strong('compares the performance of ', n.MP,
                         ' management procedures (MP) against ', n.PM,
                         ' performance metrics.')),
                       p('Each value is a median peformance metric over ', n.OM,
                                ' operating models.'),
                       p(HTML('<i class="fas fa-circle"></i>'),
                         'The', strong('large dots'), 'at the top represent the',
                         strong('average score for all'),
                         'performance metrics in each management procedure.',
                         'It provides a quick measure of overall MP performance (assumes equal weighting and equal scaling of performance metrics).'),
                       p(HTML('<i class="fas fa-circle fa-xs"></i>'),
                         'Small dots represent',
                         strong('individual scores'),
                         'for performance metrics in each management procedure.'),
                       p(HTML('<i class="fas fa-long-arrow-alt-right"></i>'),
                         'Within each row of dots, the better performing MP is toward the right.')
                     )
                     }
                   }
                 })

                 output$pg2_results <- renderPlot({
                   if(!Object$Loaded) return()
                   vert_line_plot_2(Det, MPkeep, Detkeep, SNkeep, Object$obj)
                 })

                 output$perftab <- DT::renderDataTable({
                   if(!Object$Loaded) return()
                   # select PMs and MPs
                   pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)
                   pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]

                   mp_mean <- apply(pm, 1, mean)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names

                   icon_text <- paste('<i class="fas fa-circle fa-sm" style="color:', MPcols, ';"></i>')

                   DF <- data.frame(icon=icon_text, MP=MPnames,
                                        Mean=round(mp_mean,2))
                   DF <- DF %>% dplyr::arrange(dplyr::desc(Mean))

                   colnames(DF)[1] <- ''
                   colnames(DF)[2] <- 'Management Procedure'
                   colnames(DF)[3] <- 'Overall Score (average)'

                   DT::datatable(DF, extensions = c('Responsive', 'Buttons'),
                                 escape = FALSE,
                                 options = list(
                                   paging = TRUE,
                                   searching = TRUE,
                                   fixedColumns = TRUE,
                                   autoWidth = FALSE,
                                   ordering = TRUE,
                                   dom = 'Brtip',
                                   buttons = c('copy', 'csv', 'excel'),
                                   columnDefs = list(list(className = 'dt-center', targets = 0))
                                 ),
                                 rownames=FALSE
                                 )
                 })

               }
  )
}


ZigzagUI <- function(id, label="zigzag") {
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
                              div(summaryUI(ns('page2'))
                              )
                       )
                     ),
                     fluidRow(
                       column(width = 3,
                              fluidRow(class='page_reading',
                                       column(12,
                                              h4(strong("READING THIS CHART")),
                                              htmlOutput(ns('reading'))
                                       )

                              ),
                       ),
                       column(width=8, class='left_border',
                              fluidRow(
                                shinycssloaders::withSpinner(plotOutput(ns('pg2_results'),
                                           height='650px')),
                                h3('Performance Table', class='lah'),
                                column(6,
                                       DT::dataTableOutput(ns('perftab'))
                                )
                              )
                       )
                     )
    )
  )
}

vert_line_plot_2 <- function(Det, MPkeep, Detkeep, SNkeep, obj) {

  nSN <- nrow(obj$SN$Design) # number of SN in obj
  nSNs <- sum(SNkeep$selected) # number SN selected

  nPMd <- dim(obj$Perf$Det$Values)[3] # number PM in obj
  nPMds <- sum(Detkeep$selected) # n PM selected

  nMP <- length(obj$MP$Labels) # n MPs in obj
  nMPs <- sum(MPkeep$selected) # n MPs selected

  cols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Codes[MPkeep$selected] # MP names
  Codes <- obj$Perf$Det$Codes[Detkeep$selected] # PM codes
  Values <- Det$mat[,MPkeep$selected,Detkeep$selected, drop=FALSE] # Det values

  # Calc median PM over OMs
  if (nMPs > 0 & nPMds>1) {

    pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)
    pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]

    pm.ord <- apply(pm, 2, range)
    pm.ord <- order(apply(pm.ord, 2, diff))

    mp_mean <- apply(pm, 1, mean)

    rng <- range(pm)
    min <- floor(min(rng)/0.05) * 0.05
    max <- 100
    at <- seq(min, max, by=10)
    lab <- paste0(at, '%')

    pm <- as.data.frame(pm)
    colnames(pm) <- Codes
    rownames(pm) <- MPnames
    pm <- pm[,pm.ord]
    DF <- tidyr::pivot_longer(pm, 1:ncol(pm))
    DF$MP <- rep(MPnames, each=length(Codes))
    DF$MP <- factor(DF$MP, ordered = TRUE, levels=MPnames)
    DF$name <- factor(DF$name, ordered = TRUE, levels= Codes[pm.ord])

    p1 <- ggplot(DF) +
      geom_path(size=1.1, aes(x=value, y=name, color=MP, group=MP)) +
      geom_point(size=3, aes(x=value, y=name, color=MP, group=MP)) +
      theme_bw() +
      scale_color_manual(values=cols) +
      scale_x_continuous(limits=c(0, 100), labels= function(x) paste0(x, "%")) +
      labs(x='Minimum to Maximum / Worse to Better',
           y="Performance Metrics") +
      guides(color="none") +
      expand_limits(y=c(1, nPMds+2), x=c(0,100))

    top_text <- data.frame(MP=MPnames,
                           x=mp_mean,
                           y=nPMds+1,
                           label=paste0(MPnames, '\n', round(mp_mean,1), "%"))

    p1 <- p1 + geom_point(aes(x=x, y=y, color=MP), data=top_text,
                    size=5)
    #   ggrepel::geom_text_repel(aes(x=x, y=y, label=label, color=MP),
    #                             data=top_text, size=7, fontface=2)

    label <- "bold('Overall MP Scores')~(average)"
    p1 <- p1 + annotate('text', x=50, y=nPMds+1.5, label=label,
                        parse=TRUE, size=5) +
      coord_cartesian(clip = 'off')


    # vertical lines
    df_lines <- data.frame(x=rep(mp_mean, 2),
                           y=c(rep(1, nMPs), rep(nPMds+1,nMPs)),
                           MP=rep(MPnames,2))
    p1 <- p1 + geom_line(data=df_lines,
                         aes(x=x, y=y, group=MP, color=MP), linetype='dotted')

    p1 +  theme(panel.border = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.title.x = element_text(size = axis.title,
                                            vjust=-2),
                axis.text.x = element_text(size=16,
                                           color="black"
                                           ),
                axis.title.y = element_text(size = axis.title,
                                            vjust=2),
                axis.text.y = element_text(size = axis.text),
                plot.margin = unit(c(0, 0, 0.5, 0.2), "cm")
                )
  }
}


page_2_summary <- function(Det, MPkeep, Detkeep, SNkeep, Object) {

  nSN <- nrow(Object$obj$OM$Design)
  nSNs <- sum(SNkeep$selected)

  nPMd <- dim(Object$obj$Perf$Det$Values)[3]
  nPMds <- sum(Detkeep$selected)

  nMP <- length(Object$obj$MP$Labels)
  nMPs <- sum(MPkeep$selected)

  # median PM values across OMs
  pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)

  # Highest and lowest overall score
  pm <- pm[,Detkeep$selected]
  pm.avg <- apply(pm, 1, mean, na.rm=TRUE)
  pm.avg <- pm.avg[MPkeep$selected]

  MPnames <- Object$obj$MP$Codes[MPkeep$selected]



  Highest <- paste('MP with highest average performance value:', paste(MPnames[which.max(pm.avg)], collapse=", "))
  Lowest <- paste('MP with lowest average performance value:', paste(MPnames[which.min(pm.avg)], collapse=", "))

  # Dominated MPs
  domText <- 'No MPs are outperformed by all the others.'

  DomMPs <- list()

  for (i in 1:nMP) {
    if (MPkeep$selected[i]) {
      mat <- matrix(pm[i,], nrow=nMP, ncol=nPMds, byrow=TRUE)
      dom <- apply(mat < pm, 1, prod)

      if (!any(is.na(dom)) && any(dom>0)) {
        domMP <- paste( MPnames[dom>0], collapse=", ")
        DomMPs[[i]] <- paste(MPnames[i], 'dominated by:', domMP)
      }
    }
  }
  if (any(unlist(lapply(DomMPs, length))>0)) {
    domText <- do.call('rbind', DomMPs)
    domText <- paste(domText, collapse = "\n")
  }

  paste0(c(Highest, Lowest, domText), collapse = '\n')
}


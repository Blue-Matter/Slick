# ---- Page 3 - Performance Comparison ----

# This is page 4 of the Plot_Finals.pdf
RailServer <- function(id, Det, MPkeep, Detkeep, SNkeep, Object, i18n) {
  moduleServer(id,
               function(input, output, session) {

                 output$checkloaded <- CheckLoaded(Object)

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
                         h3(i18n()$t('Rail'), id='title')
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

                 summaryServer('page3', Det, MPkeep, Detkeep, SNkeep, Object,
                               page_2_summary, minPMs=2)

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
                       p(
                         'This chart', strong('compares the performance of ', n.MP,
                         ' management procedures (MP) against ', n.PM,
                         ' performance metrics.')),
                       p(
                         'Each value is a median performance metric over ', n.OM,
                                ' operating models.'),
                       p(HTML('<i class="fas fa-circle"></i>'),
                         'The', strong('large dots'), 'at the top represent the',
                         strong('average score for all'),
                         'performance metrics in each management procedure.',
                         'It provides a quick measure of overall MP performance. These summary values assume equal weighting and equal scaling of performance metrics. '),
                       p(HTML('<i class="fas fa-circle fa-xs"></i>'),
                       'Small dots represent',
                       strong('individual scores'),
                       'for performance metrics in each management procedure.'),
                       p(HTML('<i class="fas fa-long-arrow-alt-right"></i>'),
                         'Within each row of dots, the better performing MP is toward the right.'),

                       p(HTML('<i class="fas fa-arrows-alt-h"></i>'),
                        'Performance metrics with the largest differences across',
                       'MPs are shown first as they may be key to assessing and',
                       'choosing a MP.')
                     )
                     }
                   }
                 })

                 output$results <- renderPlot({
                   if(!Object$Loaded) return()
                   horiz_line_plot(Det, MPkeep, Detkeep, SNkeep, Object$obj)
                 })

                 output$perftab <- DT::renderDataTable({
                   if(!Object$Loaded) return()
                   # select PMs and MPs
                   pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)
                   pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]

                   mp_mean <- apply(pm, 1, mean)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Labels[MPkeep$selected] # MP names

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
                                   searching = FALSE,
                                   fixedColumns = FALSE,
                                   autoWidth = FALSE,
                                   ordering = TRUE,
                                   dom = 'Brtip',
                                   pageLength=25,
                                   buttons = c('copy', 'csv', 'excel'),
                                   columnDefs = list(list(className = 'dt-center', targets = 0))
                                 )
                   )
                 })

               })

}

RailUI <- function(id, label="rail") {
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
                                summaryUI(ns('page3'))
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
                                shinycssloaders::withSpinner(plotOutput(ns('results'),
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


pg3_summary <- function() {
  # these could be the same function in all pages
}

horiz_line_plot <- function(Det, MPkeep, Detkeep, SNkeep, obj) {

  nSN <- nrow(obj$OM$Design) # number of SN in obj
  nSNs <- sum(SNkeep$selected) # number SN selected

  nPMd <- dim(obj$Perf$Det$Values)[3] # number PM in obj
  nPMds <- sum(Detkeep$selected) # n PM selected

  nMP <- length(obj$MP$Labels) # n MPs in obj
  nMPs <- sum(MPkeep$selected) # n MPs selected

  cols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Labels[MPkeep$selected] # MP names
  Codes <- obj$Perf$Det$Codes[Detkeep$selected] # PM codes
  Values <- Det$mat[,MPkeep$selected,Detkeep$selected, drop=FALSE] # Det values

  # Calc median PM over OMs
  if (nMPs > 0 & nPMds>1 & nSNs >0) {

    pm <- apply(Det$mat[SNkeep$selected,,,drop=FALSE], c(2,3), median, na.rm=TRUE)
    pm <- pm[MPkeep$selected, Detkeep$selected, drop=FALSE]

    pm.ord <- apply(pm, 2, range)
    pm.ord <- order(apply(pm.ord, 2, diff))

    # ggplot version
    DF <- data.frame(pm)
    colnames(DF) <- Codes

    DF <- tidyr::pivot_longer(DF, 1:all_of(nPMds))
    DF$MP <- rep(MPnames, each=nPMds)
    DF$MP <- factor(DF$MP, ordered = TRUE, levels=MPnames)
    DF$name <- factor(DF$name, ordered = TRUE, levels=Codes[pm.ord])

    meanDF <- DF %>% dplyr::group_by(MP) %>% dplyr::summarise(mean=mean(value), .groups='keep')
    meanDF$Y2 <- nPMds + 1
    meanDF$label <- paste0(meanDF$MP, '\n', round(meanDF$mean,1), '%')

    rngDF <- DF %>% dplyr::group_by(name) %>%
      dplyr::summarise(Min=min(value),
                        Max=max(value), .groups='keep')
    rngDF <- tidyr::pivot_longer(rngDF, cols=2:3,
                                 names_to="Range")
    rngDF2 <- data.frame(y=nPMds + 1, x=range(meanDF$mean))

    pt.size <- 5
    line.size <- 5
    mean.size <- 10

    ggplot(DF) +
      geom_line(aes(x=value, y=name), data=rngDF, color="#f2f2f2", size=line.size) +
      geom_point(aes(x=value, y=name, color=MP,group=MP), size=pt.size) +
      geom_line(aes(x=x, y=y), data=rngDF2, color='#f2f2f2', size=line.size) +
      geom_point(aes(x=mean, y=Y2, color=MP,group=MP), size=mean.size, data=meanDF) +
      # ggrepel::geom_text_repel(aes(x=mean, y=Y2, color=MP,label=label),  data=meanDF,
      #                           size=7, fontface=2, force=3) +
      # scale_y_discrete(position = "right", expand= expansion(add = c(0.5, 2)))+
      scale_y_discrete(position = "left", expand= expansion(add = c(0.5, 2)))+

      scale_x_continuous(limits=c(0, 100), labels= function(x) paste0(x, "%")) +
      annotate('text', x=50, y=nPMds+1.8, label="bold(Overall~scores)~(average)",
               parse=TRUE, size=5) +
      guides(color="none") +
      labs(x='Minimum to Maximum / Worse to Better', y='Performance Metrics', title='') +
      theme_bw() +
      scale_color_manual(values=cols) +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor = element_blank(),
            panel.border=element_blank(),
            plot.title = element_text(hjust = 0.5, size=18, face='bold'),
            axis.title.x=element_text(size=axis.title),
            axis.text.x=element_text(size=axis.text),
            axis.text.y=element_text(size=axis.text),
            axis.title.y=element_text(size=axis.title)
            )

  }

}

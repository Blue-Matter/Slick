
ViolinServer <- function(id, Stoch, MPkeep, Stochkeep, SNkeep, Object, i18n) {
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
                         h3(i18n()$t('Violin'), id='title')
                     )
                   )
                 })

                 output$subtitle <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   if (n.OM>0 & n.MP>0) {
                     str <- paste(n.MP, i18n()$t('management procedures.'),
                                  i18n()$t('Median values over'), n.OM,  i18n()$t('operating models.'))

                   } else {
                     str <-''
                   }
                   tagList(
                     div(class='page_title',
                         p(str, id='subtitle')
                     )
                   )
                 })

                 summaryServer('violin', Stoch, MPkeep, Stochkeep, SNkeep, Object,
                               violin_summary, minPMs = 2)

                 output$reading <- renderUI({
                   if(!Object$Loaded) return()
                   n.MP <- sum(MPkeep$selected)
                   n.PM <- sum(Stochkeep$selected)
                   n.OM <- sum(SNkeep$selected)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names

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
                     shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width='300px'))
                   })

                   plot_output_list$cellArgs <- list(
                   style = "
                   width: auto;
                   height: auto;
                   margin: 5px;
                   ")
                   do.call(flowLayout, plot_output_list)
                 })

                 observe({
                   nPMds <- sum(Stochkeep$selected)
                   for (i in 1:nPMds) {
                     local({
                       my_i <- i
                       plotname <- paste("plot", my_i, sep="")
                       output[[plotname]] <- renderPlot({
                         print(violinplot(Stoch, MPkeep, Stochkeep, SNkeep, PM=my_i, Object$obj))
                       }, width=300)
                     })
                   }
                 })



               }
  )
}


ViolinUI <- function(id, label="violin") {
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
                                summaryUI(ns('violin'))
                              )
                       )
                     ),
                     fluidRow(
                       column(width=3, class='page_reading',
                              div(
                                h4(strong("READING THIS CHART")),
                                htmlOutput(ns('reading'))
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

violinplot <- function(Stoch, MPkeep, Stochkeep, SNkeep, PM, obj) {
  Codes <- obj$Perf$Stoch$Codes[Stochkeep$selected] # PM codes

  nSNs <- sum(SNkeep$selected) # number SN selected
  nMPs <- sum(MPkeep$selected) # n MPs selected
  nPMds <- sum(Stochkeep$selected) # n PM selected

  MPcols <- obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- obj$MP$Codes[MPkeep$selected] # MP names
  MPnames <- factor(MPnames, ordered = TRUE, levels=MPnames)

  p <- NULL
  if (nMPs>1 & nPMds >1 & nSNs>0) {

    Values <- Stoch$mat[,SNkeep$selected, MPkeep$selected, Stochkeep$selected]

    if(!any(dim(Values)<1)) {

      dd <- dim(Values)[4]
      nsim <- dim(Values)[1]

      if (PM <= dd) {
        Val <- Values[,,,PM]
        df <- data.frame(x=rep(MPnames, each=nSNs*nsim), value=as.vector(Val))

        p <- ggplot(df, aes(x=x, y=value, fill=x)) +
          geom_violin(scale='width') +
          labs(x='', y='', title=Codes[PM]) +
          expand_limits(y=c(0,100)) +
          guides(fill='none') +
          scale_y_continuous(expand = c(0, 0)) +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.text=element_text(size=16),
                plot.title = element_text(face="bold")) +
          scale_fill_manual(values=MPcols)
      }
    }
  }
  p
}


violin_summary <-  function(Stoch, MPkeep, Stochkeep, SNkeep, Object, input) {
  Codes <- Object$obj$Perf$Stoch$Codes[Stochkeep$selected] # PM codes

  nSNs <- sum(SNkeep$selected) # number SN selected
  nMPs <- sum(MPkeep$selected) # n MPs selected
  nPMds <- sum(Stochkeep$selected) # n PM selected

  MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
  MPnames <- Object$obj$MP$Codes[MPkeep$selected] # MP names

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

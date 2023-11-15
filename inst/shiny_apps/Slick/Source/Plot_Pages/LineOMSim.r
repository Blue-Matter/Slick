

LineOMSimServer <- function(id, MPkeep, SNkeep, Object, i18n) {
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
                         h3(i18n()$t('Line OM Sim'), id='title')
                     )
                   )
                 })

                 output$subtitle <- renderUI({
                   if(!Object$Ready) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)
                   if (n.OM>0 & n.MP>0) {
                     str <- paste0(n.MP, ' management procedures. ',
                                   'Values from individual simulations over ', n.OM,  ' operating models.')
                   } else {
                     str <-''
                   }
                   tagList(
                     div(class='page_title',
                         p(str, id='subtitle')
                     )
                   )
                 })

                 dummyKeep <- list()
                 dummyKeep$selected <- 1:3
                 dummyDet <- list()
                 summaryServer('lineOMSim', dummyDet, MPkeep, dummyKeep, SNkeep, Object,
                               lineOM_summary, minPMs=3, input)


                 output$reading <- renderUI({
                   if(!Object$Ready) return()
                   n.MP <- sum(MPkeep$selected)
                   n.OM <- sum(SNkeep$selected)

                   n.sim <- dim(Object$obj$StateVar$Values)[1]

                   if (n.MP>0 & n.OM>0) {
                     tagList(
                       p(
                         'This chart compares ',
                         strong('projected stock status variables over time (selectable under State Variable dropdown at right) for ',
                                n.MP, ' management procedures'),
                         'in an individual simulation by', strong(
                           n.OM, 'operating models. Select the simulation number in the dropdown menu.'
                         )
                       ),
                       p('Target and limit reference points are shown in green and red, respectively, if they have been specified.')
                     )
                   }
                 })

                 output$SV_dropdown <- renderUI({
                   if(!Object$Ready) return()
                   tagList(
                     h4('Select State Variable for Projection'),
                     selectInput(session$ns('selectSV'),
                                 'State Variable',
                                 choices=Object$obj$StateVar$Labels)
                   )
                 })

                 output$SV_sim <- renderUI({
                   if(!Object$Ready) return()
                   n.sim <- dim(Object$obj$StateVar$Values)[1]
                   tagList(
                     h4('Select Simulation Number'),
                     selectInput(session$ns('selectSim'),
                                 '',
                                 choices=seq_len(n.sim))
                   )
                 })

                 output$SV_Yrange <- renderUI({
                   ymax <- maxVal <- 4
                   if(!Object$Ready) return()

                   SV_ind <- unlist(Object$obj$StateVar$Labels) == input$selectSV

                   nSNs <- sum(SNkeep$selected) # number SN selected
                   nMPs <- sum(MPkeep$selected) # n MPs selected
                   nSVs <- sum(SV_ind)

                   if (nMPs>0 & nSVs>0 & nSNs>0) {
                     maxVal <- quantile(Object$obj$StateVar$Values[,SNkeep$selected, MPkeep$selected, SV_ind, , drop=FALSE], 0.95)
                     ymax <- roundUpNice(maxVal)
                   }

                   tagList(
                     h4('Set Y Axis'),
                     sliderInput(session$ns('yaxis'),
                                 'Range',
                                 0,
                                 ymax,
                                 c(0, maxVal))
                   )
                 })

                 output$lineOMSim_plot <- renderUI({
                   if(!Object$Ready) return()
                   n.SN <- sum(SNkeep$selected)
                   SN.select <- which(SNkeep$selected)
                   plot_output_list <- lapply(SN.select, function(mm) {
                     plotname <- paste("plot_lineOMSim", mm, sep="")
                     tagList(
                       shinycssloaders::withSpinner(plotOutput(session$ns(plotname), width='550px', height='400px'))
                     )

                   })
                   plot_output_list$cellArgs <- list(
                     style = "
                     width: 550px;
                     height: 450px;
                           "
                   )
                   do.call(flowLayout, plot_output_list)

                 })

                 observe({
                   n.SN <- sum(SNkeep$selected)
                   SN.select <- which(SNkeep$selected)
                   for (i in 1:length(SNkeep$selected)) {
                     if (i %in% SN.select) {
                       local({
                         my_i <- i
                         plotname <- paste("plot_lineOMSim", my_i, sep="")
                         output[[plotname]] <- renderPlot({
                           MP_projection_OM(MPkeep, input, sn=my_i, Object$obj, by_sim = TRUE)
                         })
                       })
                     }

                   }
                 })



                 output$MPlist <- renderUI({
                   if(!Object$Ready) return()
                   n.MP <- sum(MPkeep$selected)

                   MPcols <- Object$obj$Misc$Cols$MP[MPkeep$selected] # MP colors
                   MPnames <- Object$obj$MP$Labels[MPkeep$selected] # MP names

                   # write css class
                   text <- paste0("<p> <b class='horizline' style=' border-top: .3rem solid ", MPcols, ";'></b>",
                                  MPnames, "</p>")

                   text <- paste(text, collapse=" ")

                   tagList(
                     HTML(text)
                   )

                 })


               }
  )
}




Line_OMSimUI <- function(id, label="lineOMSim") {
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
                                summaryUI(ns('lineOMSim'))
                              )
                       )
                     ),
                     fluidRow(
                       column(width=3,
                              br(),
                              h4(strong("Management Procedure")),
                              htmlOutput(ns('MPlist'))
                       ),
                       column(width=3,  class='page_reading',
                              h4(strong("READING THIS CHART")),
                              htmlOutput(ns('reading'))
                       ),
                       column(width=3,
                              #img(src='img/LineOM.jpg', width='100%')
                       ),
                       fluidRow(
                         column(width=4,
                                br(),
                                htmlOutput(ns('SV_dropdown')),
                                br(),
                                htmlOutput(ns('SV_Yrange'))
                         ),
                         column(width=4,
                                br(),
                                htmlOutput(ns('SV_sim'))
                         )
                       )
                     ),
                     fluidRow(
                       column(width=12, class='top_border',
                              htmlOutput(ns('lineOMSim_plot'))
                       )
                     )
    )
  )
}


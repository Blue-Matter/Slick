
LoadServer <- function(id, Object, i18n) {
  moduleServer(id,
               function(input, output, session) {

                 ns <- session$ns

                 output$load <- renderUI({
                   box(title=h2(i18n()$t('Load a Slick Data File')),
                       width=6,
                       solidHeader=TRUE,
                       collapsible = TRUE,
                       status = "primary",
                       h4(i18n()$t('Instructions')),
                       p(i18n()$t('Load your own Slick Data file or choose an example from the dropdown menu.')),
                       p(i18n()$t('Once a Slick Data file is loaded, additional navy blue boxes will appear summarizing the contents of the Slick Data file. Most of the content in the navy blue boxes are loaded directly from the Slick Data file and subsequently will not be updated when the Slick language setting is changed.')),
                       # upload slick file
                       box(title=h4(i18n()$t('Load your MSE Results')),
                           height='400px',
                           p(i18n()$t('Select the Slick Data File containing your MSE results and upload. For more information on creating a Slick Data file see the '), a(href='https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html', 'Slick Developer Guide.', target="_blank")),
                           fileInput("Load", accept=c("slick",".slick"),
                                     label = i18n()$t("From file (.slick)"),
                                     buttonLabel=list(icon("folder",verify_fa = FALSE))
                           )
                       ),
                       box(title=h4(i18n()$t('Load an Example')),
                           height='400px',
                           p(i18n()$t('Select an Example from the dropdown menu and click `Load`. Click `Download` to download the Example Slick Data file as an R object. The downloaded R object can be imported into R with `readRDS`.')),

                           selectInput('example_input',
                                       label=i18n()$t('Example'),
                                       choices=c('Demonstration',
                                                 'Atlantic bluefin tuna',
                                                 'North Atlantic swordfish'
                                       ),
                                       selected=NULL
                           ),
                           actionButton("example_upload", i18n()$t("Load"),
                                         icon("upload", verify_fa = FALSE)),
                           downloadButton("example_download", i18n()$t("Download"),
                                        icon("cloud-download", verify_fa = FALSE))

                       )
                   )

                 })



                 output$metadata <- renderUI({
                   tagList(
                     box(width=6,
                         solidHeader=TRUE,
                         collapsible = TRUE,
                         status = "navy",
                         title=h2(i18n()$t('Metadata')),
                         h3(Object$obj$Text$Title),
                         h4(Object$obj$Text$Sub_title),
                         p(HTML(i18n()$t('<strong>Fishery:</strong>')), Object$obj$name),
                         p(HTML(i18n()$t('<strong>Author:</strong>')), Object$obj$Misc$Author),
                         p(HTML(i18n()$t('<strong>Contact:</strong>')), Object$obj$Misc$Contact),
                         p(HTML(i18n()$t('<strong>Institution:</strong>')), Object$obj$Misc$Institution),
                         lapply(Object$obj$Text$Introduction, tags$p)
                     )
                   )
                 })


                 output$summary <- renderUI({
                   if (Object$Ready) {
                     tagList(tabsetPanel(type = "tabs",
                                         # tabPanel(i18n()$t("About"), br(),
                                                  # DT::dataTableOutput(session$ns('metadata'))),
                                         tabPanel(i18n()$t("Management Procedures"),br(), DT::dataTableOutput(session$ns('MPs'))),
                                         tabPanel(i18n()$t("Operating Model"), br(),
                                                  renderUI({
                                                    tagList(tabsetPanel(type="tabs",
                                                                        tabPanel('Factors',
                                                                                 br(), DT::dataTableOutput(session$ns('OMs'))),
                                                                        tabPanel('Design',
                                                                                 br(), DT::dataTableOutput(session$ns('OMDes')))
                                                    ))
                                                  })
                                         ),
                                         tabPanel(i18n()$t("Performance Metrics"), br(),
                                                  renderUI({
                                                    tagList(tabsetPanel(type = "tabs",
                                                                        tabPanel(i18n()$t("Deterministic"),
                                                                                 br(),DT::dataTableOutput(session$ns('PM_Det'))),
                                                                        tabPanel(i18n()$t("Stochastic"),
                                                                                 br(),DT::dataTableOutput(session$ns('PM_Stoch'))),
                                                                        tabPanel(i18n()$t("Projection"),
                                                                                 br(),DT::dataTableOutput(session$ns('PM_Proj')))
                                                    )
                                                    )
                                                  })
                                         )

                     )
                     )
                   }
                 })



                 output$MPs <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(
                     Code=Object$obj$MP$Codes,
                     Label=Object$obj$MP$Labels,
                     Description=Object$obj$MP$Description
                   )
                   DT::datatable(df,rownames=F, extensions = 'Responsive', selection='none')
                 })

                 output$OMs <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(Factor =  rep(Object$obj$OM$Factor_Labels,unlist(lapply(Object$obj$OM$Codes,FUN=function(x)length(x)))),
                                    Level = unlist(Object$obj$OM$Codes),
                                    Description = unlist(Object$obj$OM$Description))
                   DT::datatable(df, extensions = 'Responsive', selection='none')
                 })

                 output$OMDes <- renderDataTable({
                   if(!Object$Ready) return()
                   df<-array(NA,c(nrow(Object$obj$OM$Design),ncol=ncol(Object$obj$OM$Design))) # weird code but is robust to matrix input and data.frame input
                   df<-data.frame(df)
                   names(df)<-Object$obj$OM$Factor_Labels
                   for(i in 1:ncol(df))  df[,i]<-Object$obj$OM$Codes[[i]][Object$obj$OM$Design[,i]]
                   DT::datatable(df, extensions = 'Responsive', selection='none')
                 })

                 output$PM_Det <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame( Code=Object$obj$Perf$Det$Codes,
                                     Label=Object$obj$Perf$Det$Labels,
                                     Description=Object$obj$Perf$Det$Description
                   )
                   DT::datatable(df,rownames=F,extensions = 'Responsive', selection='none')
                 })

                 output$PM_Stoch <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame( Code=Object$obj$Perf$Stoch$Codes,
                                     Label=Object$obj$Perf$Stoch$Labels,
                                     Description=Object$obj$Perf$Stoch$Description
                   )
                   DT::datatable(df,rownames=F,extensions = 'Responsive', selection='none')
                 })

                 output$PM_Proj <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame( Code=Object$obj$Perf$Proj$Codes,
                                     Label=Object$obj$Perf$Proj$Labels,
                                     Description=Object$obj$Perf$Proj$Description
                   )
                   DT::datatable(df,rownames=F,extensions = 'Responsive', selection='none')
                 })

                 output$download <- renderUI({
                   if(!Object$Ready) return()
                   tagList(
                     downloadButton('downloadData', 'Download')
                   )
                 })




               }
  )
}



LoadUI <- function(id, label="load") {

  ns <- NS(id)

  tagList(
    usei18n(i18n),
    fluidRow(
      htmlOutput(ns('load')),
      # htmlOutput(ns('load'))
      conditionalPanel('output.Loaded>0',
                       uiOutput(ns('metadata')),

                       box(width=12,
                           solidHeader=TRUE,
                           status = "primary",
                           title=h2(uiOutput(ns("Title"))),
                           h5( uiOutput(ns("Subtitle"))),
                           p( uiOutput(ns("Intro1"))),
                           p( uiOutput(ns("Intro2"))),
                           p( uiOutput(ns("Intro3"))),
                           uiOutput(ns('summary'))
                       )
      )

    )
  )
}

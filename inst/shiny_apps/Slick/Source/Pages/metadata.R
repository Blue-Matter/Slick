
metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('main'))
  )
}

metadataServer <- function(id, Object, i18n) {
  ns <- NS(id)

  moduleServer(id,
               function(input, output, session) {
                 # mod_links_server(id, Object, i18n)
                 output$main <- renderUI({
                   i18n <- i18n()
                   tagList(
                     shinydashboardPlus::box(title=h3(strong(i18n$t('Overview'))),
                                             width=12,
                                             solidHeader=TRUE,
                                             status = "primary",
                                             uiOutput(ns('tabsetpanel'))
                     )
                     # mod_links_ui(ns(id))
                   )
                 })

                 # main tabBox panels ----
                 output$tabsetpanel <- renderUI({
                   i18n <- i18n()
                   tagList(
                     fluidRow(
                       column(12,
                              tabBox(width=12,
                                     tabPanel(title=h4(strong(i18n$t('Metadata'))),
                                              uiOutput(ns('metadata'))

                                     ),
                                     tabPanel(title=h4(strong(i18n$t('Management Procedures'))),
                                              uiOutput(ns('management_procedures'))

                                     ),
                                     tabPanel(title=h4(strong(i18n$t('Operating Models'))),
                                              uiOutput(ns('operating_models'))

                                     ),
                                     tabPanel(title=h4(strong(i18n$t('Performance Metrics'))),
                                              uiOutput(ns('performance_metrics'))

                                     )
                              )
                       )

                     )

                   )
                 })

                 # ---- metadata tab ----

                 output$metadata <- renderUI({
                   i18n <- i18n()
                   tagList(
                     column(12,
                            br(),
                            h3(Object$obj$Text$Title),
                            h4(Object$obj$Text$Sub_title),
                            br(),
                            p(strong(i18n$t('Fishery:')), Object$obj$Misc$Fishery),

                            p(strong(i18n$t('Author(s):')), Object$obj$Misc$Author, HTML(Object$obj$Misc$Contact)),
                            p(strong(i18n$t('Institution(s):')), Object$obj$Misc$Institution),
                            p(strong(i18n$t('Created:')), Object$obj$Misc$Date),
                            lapply(lapply(Object$obj$Text$Introduction, HTML), tags$p)
                     )
                   )
                 })

                 # ---- MP tab ----
                 output$management_procedures <- renderUI({
                   i18n <- i18n()
                   tagList(
                     column(12,
                            br(),
                            h3(i18n$t('Management Procedures')),
                            DT::dataTableOutput(session$ns('MPs'))
                     )
                   )
                 })

                 output$MPs <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(
                     # Code=Object$obj$MP$Codes,
                     Label=Object$obj$MP$Labels,
                     Description=Object$obj$MP$Description
                   )
                   DT::datatable(df,rownames=F, extensions = 'Responsive',
                                 selection='none', options = list(dom = 't',
                                                                  pageLength=100))
                 })

                 # ---- OM tab ----
                 output$operating_models <- renderUI({
                   i18n <- i18n()
                   tagList(
                     column(12,
                            br(),
                            h3(i18n$t('Operating Models')),
                            tabsetPanel(type="tabs",
                                        tabPanel('Factors',
                                                 br(), DT::dataTableOutput(session$ns('OMs'))),
                                        tabPanel('Design',
                                                 br(), DT::dataTableOutput(session$ns('OMDes')))
                            )
                     )
                   )
                 })

                 output$OMs <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(Factor =  rep(Object$obj$OM$Factor_Labels,unlist(lapply(Object$obj$OM$Codes,FUN=function(x)length(x)))),
                                    Level = unlist(Object$obj$OM$Codes),
                                    Description = unlist(Object$obj$OM$Description))
                   DT::datatable(df, extensions = 'Responsive', selection='none', options = list(dom = 't',
                                                                                                 pageLength=100))
                 })

                 output$OMDes <- renderDataTable({
                   if(!Object$Ready) return()
                   df<-array(NA,c(nrow(Object$obj$OM$Design),ncol=ncol(Object$obj$OM$Design))) # weird code but is robust to matrix input and data.frame input
                   df<-data.frame(df)
                   names(df)<-Object$obj$OM$Factor_Labels
                   for(i in 1:ncol(df))  df[,i]<-Object$obj$OM$Codes[[i]][Object$obj$OM$Design[,i]]
                   DT::datatable(df, extensions = 'Responsive', selection='none', options = list(dom = 't',
                                                                                                 pageLength=100))
                 })

                 # ---- PM tab ----

                 output$performance_metrics <- renderUI({
                   i18n <- i18n()
                   tagList(
                     column(12,
                            br(),
                            h3(i18n$t('Performance Metrics')),
                            tabsetPanel(type = "tabs",
                                        tabPanel(i18n$t("Deterministic"),
                                                 br(),DT::dataTableOutput(session$ns('PM_Det'))),
                                        tabPanel(i18n$t("Stochastic"),
                                                 br(),DT::dataTableOutput(session$ns('PM_Stoch'))),
                                        tabPanel(i18n$t("Projection"),
                                                 br(),DT::dataTableOutput(session$ns('PM_Proj')))
                            )
                     )
                   )
                 })


                 output$PM_Det <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(Code=Object$obj$Perf$Det$Codes,
                                    # Label=Object$obj$Perf$Det$Labels,
                                    Description=Object$obj$Perf$Det$Description
                   )
                   DT::datatable(df,rownames=F,extensions = 'Responsive',
                                 selection='none',
                                 options =  list(dom = 't',  pageLength=100),
                                 escape = FALSE)
                 })

                 output$PM_Stoch <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(Code=Object$obj$Perf$Stoch$Codes,
                                    # Label=Object$obj$Perf$Stoch$Labels,
                                    Description=Object$obj$Perf$Stoch$Description
                   )
                   DT::datatable(df,rownames=F,extensions = 'Responsive',
                                 selection='none',
                                 options = list(dom = 't', pageLength=100),
                                 escape = FALSE)
                 })

                 output$PM_Proj <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(Code=Object$obj$Perf$Proj$Codes,
                                    # Label=Object$obj$Perf$Proj$Labels,
                                    Description=Object$obj$Perf$Proj$Description
                   )
                   DT::datatable(df,rownames=F,extensions = 'Responsive',
                                 selection='none', options = list(dom = 't',
                                                                  pageLength=100),
                                 escape = FALSE)
                 })

               })
}

#
# tags$li(
#   strong(i18n$t('Filter the Results:')),
#   p(i18n$t('The Filter button in the top right corner can be used to filter the MSE results shown in the plots. Click the Filter button to expand the Filter control menu')
#   )
# )

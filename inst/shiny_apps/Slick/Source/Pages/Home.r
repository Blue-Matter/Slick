
HomeServer <- function(id, Object, i18n) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)

                 print(i18n$translate('Hello'))
                 shiny.i18n::update_lang(session, 'es')

                 output$summary <- renderUI({
                   if (Object$Ready) {
                     tagList(tabsetPanel(type = "tabs",
                                         tabPanel(i18n$t("About"), br(),DT::dataTableOutput(session$ns('metadata'))),
                                         tabPanel(i18n$t("Management Procedures"),br(), DT::dataTableOutput(session$ns('MPs'))),
                                         tabPanel(i18n$t("Operating Model"), br(),
                                                  renderUI({
                                                    tagList(tabsetPanel(type="tabs",
                                                                        tabPanel('Factors',
                                                                                 br(), DT::dataTableOutput(session$ns('OMs'))),
                                                                        tabPanel('Design',
                                                                                 br(), DT::dataTableOutput(session$ns('OMDes')))
                                                    ))
                                                  })
                                         ),
                                         tabPanel(i18n$t("Performance Metrics"), br(),
                                                  renderUI({
                                                    tagList(tabsetPanel(type = "tabs",
                                                                        tabPanel(i18n$t("Deterministic"),
                                                                                 br(),DT::dataTableOutput(session$ns('PM_Det'))),
                                                                        tabPanel(i18n$t("Stochastic"),
                                                                                 br(),DT::dataTableOutput(session$ns('PM_Stoch'))),
                                                                        tabPanel(i18n$t("Projection"),
                                                                                 br(),DT::dataTableOutput(session$ns('PM_Proj')))
                                                    )
                                                    )
                                                  })
                                         )

                     )
                     )
                   }
                 })

                 output$metadata <- renderDataTable({
                   if(!Object$Ready) return()
                   df <- data.frame(Fishery=Object$obj$name,
                                    Author=Object$obj$Misc$Author,
                                    Contact=Object$obj$Misc$Contact,
                                    Institution=Object$obj$Misc$Institution
                   )

                   DT::datatable(df, extensions = 'Responsive',
                                 rownames=F,
                                 options = list(
                                   paging = FALSE,
                                   searching = FALSE,
                                   fixedColumns = FALSE,
                                   autoWidth = FALSE,
                                   ordering = FALSE,
                                   dom = 't'),
                                 selection='none'
                   )
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

                 output$Title <- renderText(Object$obj$Text$Title)
                 output$Subtitle <- renderText(Object$obj$Text$Sub_title)
                 output$Intro1 <- renderText(Object$obj$Text$Introduction[[1]])
                 output$Intro2 <- renderText(Object$obj$Text$Introduction[[2]])
                 output$Intro3 <- renderText(Object$obj$Text$Introduction[[3]])

               }
  )
}



HomeUI <- function(id, label="load", i18n) {

  ns <- NS(id)

  tagList(
    usei18n(i18n),
    fluidRow(

      # welcome
      box(title=h2(i18n$t('Welcome')),
          width=6,
          solidHeader=TRUE,
          status = "primary",
          h3(i18n$t('Welcome to Slick')),
          p(i18n$t('Slick is a decision analysis tool that presents the outcomes of candidate harvest strategies across various states of nature, accounting for uncertainty. Slick is interactive and allows users to filter results live in order to explore robustness and performance.')),
          p(i18n$t('While Slick can be applied to any decision analysis context it was specifically designed to investigate the performance of harvest strategies tested by management strategy evaluation (MSE).')),
          h3(i18n$t('How to use Slick')),
          p(i18n$t('Instructions here ...'))
      ),

      # load object box
      box(title=h2(i18n$t('Load')),
          width=6,
          solidHeader=TRUE,
          status = "primary",
          h3(i18n$t('Load a Slick Object')),
          p(i18n$t('Load a Slick object from file, or choose an example Slick object from the Example drop-down and click "Select"')),

          # upload slick file
          column(6,
                 fileInput("Load", accept=c("slick",".slick"),
                           label = i18n$t("From file (.slick)"),
                           buttonLabel=list(icon("folder"))
                 )
          ),
          # load example
          column(6,
                 fluidRow(
                   selectInput('example_input',
                               label=i18n$t('Example'),
                               choices=c('Demonstration',
                                         'Atlantic bluefin tuna',
                                         'North Atlantic swordfish'
                               ),
                               selected=NULL
                   ),
                   actionButton("example_upload", i18n$t("Select"),
                                icon("cloud-upload"))
                 )
          ),
          footer=tagList(
            p(i18n$t('For more on using Slick, please see the: '),
              shiny::actionButton(inputId='ab1', label="User Guide",
                                  icon = icon("question-circle"),
                                  onclick ="window.open('https://blue-matter.github.io/openMSE/Slick-User-Guide.html', '_blank')"),
              shiny::actionButton(inputId='ab1', label="Developers' Manual",
                                  icon = icon("info-circle"),
                                  onclick ="window.open('https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html', '_blank')")
            )
          )
      ),
      fluidRow(
        column(12,
               conditionalPanel('output.Loaded>0',
                                hr(),
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
    )
  )

}

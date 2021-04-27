

SplashServer <- function(id, Object) {
  moduleServer(id,
               function(input, output, session) {

                 output$summary <- renderUI({
                   if (Object$Ready) {
                     tagList( tabsetPanel(type = "tabs",
                                          tabPanel("About", br(),DT::dataTableOutput(session$ns('metadata'))),
                                          tabPanel("Management Procedures",br(), DT::dataTableOutput(session$ns('MPs'))),
                                          tabPanel("Operating Model", br(),
                                                   renderUI({
                                                     tagList(tabsetPanel(type="tabs",
                                                                         tabPanel('Factors',
                                                                                  br(), DT::dataTableOutput(session$ns('OMs'))),
                                                                         tabPanel('Design',
                                                                                  br(), DT::dataTableOutput(session$ns('OMDes')))
                                                                         ))
                                                   })
                                                   ),
                                          tabPanel("Performance Metrics", br(),
                                                   renderUI({
                                                     tagList(tabsetPanel(type = "tabs",
                                                                         tabPanel("Deterministic",
                                                                                  br(),DT::dataTableOutput(session$ns('PM_Det'))),
                                                                         tabPanel("Stochastic",
                                                                                  br(),DT::dataTableOutput(session$ns('PM_Stoch'))),
                                                                         tabPanel("Projection",
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
                                   dom = 't',
                                   selection='none')
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



SplashUI <- function(id, label="splash") {

  ns <- NS(id)

  tagList(
    fluidRow(
      column(12, h4('How to use Slick')),
      column(12,
             p('Choose a Slick object from the Example drop-down and click "Select" or load a Slick object from file.'),
             p('Then click through tabs to view results. For more on using Slick, please see the: ',
             shiny::actionButton(inputId='ab1', label="User Guide",
                                 icon = icon("question-circle"),
                                 onclick ="window.open('https://blue-matter.github.io/openMSE/Slick-User-Guide.html', '_blank')"),
             shiny::actionButton(inputId='ab1', label="Developers' Manual",
                                 icon = icon("info-circle"),
                                 onclick ="window.open('https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html', '_blank')")
             )
      ),


      column(12, hr()),

      column(12,
             column(9,

               column(12, h4('Load')),

               column(4,selectInput('example_input',
                           label='Example',
                           choices=c('Demonstration',
                                     'Atlantic bluefin tuna',
                                     'North Atlantic swordfish'
                                     ),
                           selected=NULL
                        )),
               column(2,style="padding-top:25px",
                        actionButton("example_action", "Select",
                              icon("cloud-upload"))),

               column(1),
               column(5, fileInput("SplashLoad",
                                   accept=c("slick",".slick"),
                                   label = "From file (.slick)",
                                   buttonLabel=list(icon("folder"))
               ))
            )
            # ,

            # column(3,
            #  conditionalPanel('output.Loaded>0',
            #     column(12,
            #            h4('Save'),
            #            p('Download the Slick object:'),
            #            style="padding-top:25px",uiOutput(ns('download'))
            #            )
            #  )
            # )

      ),

    ),


    fluidRow(
      #tags$style(HTML("#Title{font-size:14; font-style:italic;}")),
      column(12,
        conditionalPanel('output.Loaded>0',
          hr(),
          column(12,
            h4( uiOutput(ns("Title"))),
            h5( uiOutput(ns("Subtitle"))),
            p( uiOutput(ns("Intro1"))),
            p( uiOutput(ns("Intro2"))),
            p( uiOutput(ns("Intro3"))),
            uiOutput(ns('summary'))

          )

        )
      )

    ),

    column(12,br())
  )
}

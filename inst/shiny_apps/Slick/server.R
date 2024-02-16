

options(shiny.maxRequestSize=100000*1024^2)


server <- function(input, output, session) {

  # -- multi-language support ----
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })

  output$language <- renderUI({
    tagList(
      selectInput('selected_language',
                  "Select language",
                  choices = languages,
                  selected = translator$get_key_translation()
      )
    )
  })

  observeEvent(input$selected_language, {
     shiny.i18n::update_lang(input$selected_language, session)
  }, ignoreInit = TRUE)

  # -- about menu ----
  output$about <- renderUI({
    tagList(
      h4(i18n()$t('About Slick')),
      p(i18n()$t('Slick was developed by'), a(href='https://www.bluematterscience.com/', 'Blue Matter Science',  target="_blank"),
        i18n()$t("and designed and commissioned by"), a(href='https://oceanfdn.org/', "The Ocean Foundation's",  target="_blank"),
        i18n()$t('International Fisheries Conservation Project and'), a(href='www.harveststrategies.org', "www.harveststrategies.org,",  target="_blank"),
        i18n()$t('with support from'), a(href='https://www.pewtrusts.org/', 'The Pew Charitable Trusts,',  target="_blank"),
        i18n()$t('and the'),  a(href='https://www.fao.org/in-action/commonoceans/what-we-do/tuna/en/', 'Common Oceans Tuna Fisheries Project,',  target="_blank"),
        i18n()$t('which is funded by'),  a(href='https://www.thegef.org/what-we-do/topics/areas-beyond-national-jurisdiction', 'GEF',  target="_blank"),
        i18n()$t('and implemented by the'),  a(href='https://www.fao.org/in-action/commonoceans/en/', 'FAO.',  target="_blank")),
      p(i18n()$t('The prototype figure designs were developed by'), a(href="https://www.5wgraphics.com/",  '5W Infographics.',  target="_blank")),
      p(i18n()$t('Slick is under going further development. All feedback is welcome. Please contact'),
        a(href="mailto:smiller@oceanfdn.org?&subject=Slick Development", 'Shana Miller'),
        i18n()$t('with any comments or suggestions for further development.')),
      br(),
      fluidRow(align = "center",
               column(4,
                      tags$a(href='https://harveststrategies.org/', target="_blank",
                             tags$div(
                               tags$img(src='img/logos/HSlogo_C_RGB_round_small.png', height = '100', width ='100'),
                               p('Harveststrategies.org')
                             )
                      )
               ),
               column(1),
               column(4,
                      tags$a(href='https://www.bluematterscience.com/', target="_blank",
                             tags$div(
                               tags$img(src='img/logos/Blue Matter colour.png', height = '114', width ='216')

                             )
                      )
               )
      ),
      br()
      )
  })



  output$MPs <- renderDataTable({
    if(!Object$Ready) return()
    df <- data.frame(
      # Code=Object$obj$MP$Codes,
      Label=Object$obj$MP$Labels,
      Description=Object$obj$MP$Description
    )
    DT::datatable(df,rownames=F, extensions = 'Responsive', selection='none', options = list(dom = 't',
                                                                                             pageLength=100))
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

  output$PM_Det <- renderDataTable({
    if(!Object$Ready) return()
    df <- data.frame(Code=Object$obj$Perf$Det$Codes,
                     #Label=Object$obj$Perf$Det$Labels,
                     Description=Object$obj$Perf$Det$Description
    )
    DT::datatable(df,rownames=F,extensions = 'Responsive', selection='none', options = list(dom = 't',
                                                                                            pageLength=100), escape = FALSE)
  })

  output$PM_Stoch <- renderDataTable({
    if(!Object$Ready) return()
    df <- data.frame(Code=Object$obj$Perf$Stoch$Codes,
                    # Label=Object$obj$Perf$Stoch$Labels,
                     Description=Object$obj$Perf$Stoch$Description
    )
    DT::datatable(df,rownames=F,extensions = 'Responsive', selection='none', options = list(dom = 't',
                                                                                            pageLength=100), escape = FALSE)
  })

  output$PM_Proj <- renderDataTable({
    if(!Object$Ready) return()
    df <- data.frame(Code=Object$obj$Perf$Proj$Codes,
                     #Label=Object$obj$Perf$Proj$Labels,
                     Description=Object$obj$Perf$Proj$Description
    )
    DT::datatable(df,rownames=F,extensions = 'Responsive', selection='none', options = list(dom = 't',
                                                                                            pageLength=100),  escape = FALSE)
  })


  output$mp_details <- renderUI({
    tagList(
      shinydashboardPlus::box(width=12,
          status = "navy",
          title=h3(i18n()$t('Management Procedures')),
          DT::dataTableOutput(session$ns('MPs'))
      )
    )
  })

  output$om_details <- renderUI({
    tagList(
      shinydashboardPlus::box(width=12,
          status = "navy",
          title=h3(i18n()$t('Operating Models')),
          tabsetPanel(type="tabs",
                      tabPanel('Factors',
                               br(), DT::dataTableOutput(session$ns('OMs'))),
                      tabPanel('Design',
                               br(), DT::dataTableOutput(session$ns('OMDes')))
          )
      )
    )
  })

  output$pm_details <- renderUI({
    tagList(
      box(width=12,
          title=h3(i18n()$t('Performance Metrics')),
          tabsetPanel(type = "tabs",
                      tabPanel(i18n()$t("Deterministic"),
                               br(),DT::dataTableOutput(session$ns('PM_Det'))),
                      tabPanel(i18n()$t("Stochastic"),
                               br(),DT::dataTableOutput(session$ns('PM_Stoch'))),
                      tabPanel(i18n()$t("Projection"),
                               br(),DT::dataTableOutput(session$ns('PM_Proj')))
          )
      )
    )
  })


  # -- Initialize Reactive Values -----
  # load slick object
  Object <- reactiveValues(Loaded=FALSE,
                           File=NULL,
                           obj=NULL,
                           nFac=1,
                           nsim=1,
                           nMP=1,
                           nPMd=1,
                           nPMs=1,
                           nPMp=1,
                           Ready=FALSE,
                           Filt=FALSE)

  output$Loaded <- reactive({ Object$Loaded })
  outputOptions(output, "Loaded", suspendWhenHidden = FALSE)

  output$Filt <- reactive({ Object$Filt })
  outputOptions(output, "Filt", suspendWhenHidden = FALSE)

  # window_dims <<- reactive(input$dimension)
  window_dims <- reactive(input$dimension)

  # # Non technical arrays
  Det <- reactiveValues(mat=array())
  Stoch <- reactiveValues(mat=array())
  Proj <- reactiveValues(mat=array())

  # Selections
  SNkeep <- reactiveValues(selected=T)

  MPkeep <- reactiveValues(selected=T)
  Detkeep <- reactiveValues(selected=T)
  Stochkeep <- reactiveValues(selected=T)
  Projkeep <- reactiveValues(selected=T)


  # Log (currently not used)
  Log_text <- reactiveValues(text="nothing happened yet")

  # -- Observe Events -----
  observeEvent(input$Load, {
    Object$File <- input$Load
    Object$Loaded <- Object$Loaded + 1

  })

  observeEvent(input$example_upload, {
    Object$File <- input$example_input
    Object$Loaded <- Object$Loaded + 1
  })


  output$example_download <- downloadHandler(
    filename = function() {
      Name <- input$example_input
      paste0(Name, ".slick", sep="")
    },
    content = function(file) {
      Name <- input$example_input
      File <- file.path('./data/case_studies', case_study_df$File[match(Name, case_study_df$Example)])
      file.copy(File, file)
    }
  )

  observeEvent(Object$Loaded, {
    if (Object$Loaded >= 1) {
      Object$Ready <- FALSE
      Object$Filt <- FALSE

     # load the object
      if (inherits(Object$File, 'character')) {
        obj <- readRDS(file.path('./data/case_studies', case_study_df$File[match(Object$File, case_study_df$Example)]))
      }

      if (inherits(Object$File, 'data.frame')) {
        obj <- readRDS(Object$File$datapath)

      }

      # ---- Run checks on loaded object ----

      # - Default Colors -
      nMP <- length(obj$MP$Labels)
      ncol <- length(obj$Misc$Cols$MP)
      if (ncol< nMP) {
        cols <- grDevices::topo.colors(nMP) # dumb range of colors
        obj$Misc$Cols$MP <- cols # c(obj$Misc$Cols$MP, cols)[1:nMP]
      }

      # other checks and error messages ...

      # - Store obj as reactive variable -
      Object$obj <- obj

      # - update dimensions -
      Object$nFac <- ncol(obj$OM$Design)
      Object$nsim <- dim(obj$Perf$Stoch$Values)[1]
      Object$nSN <- nrow(obj$OM$Design)
      Object$nMP <- length(obj$MP$Labels)
      Object$nPMd <- dim(obj$Perf$Det$Values)[3]
      Object$nPMs <- dim(obj$Perf$Stoch$Values)[4]
      Object$nPMp <- dim(obj$Perf$Proj$Values)[4]

      # - update reactive values -
      SNkeep$selected <- rep(T,Object$nSN)

      # Default OMs
      if (!is.null(obj$OM$Defaults)) {
        defaults <- obj$OM$Design
        defaults[] <- 0
        for (i in 1:Object$nFac) {
          ind <-obj$OM$Design[,i] %in% selectedOMs(i, obj$OM)
          defaults[,i] <- ind
        }
        SNkeep$selected <- as.logical(apply(defaults,1, prod))
      }

      MPkeep$selected <- rep(T,Object$nMP)
      Detkeep$selected <- rep(T,Object$nPMd)
      Stochkeep$selected <- rep(T,Object$nPMs)
      Projkeep$selected <- rep(T,Object$nPMp)

      Det$mat <- obj$Perf$Det$Values
      Stoch$mat <-obj$Perf$Stoch$Values
      Proj$mat <-obj$Perf$Proj$Values

      Object$Ready <- TRUE

      #saveRDS(reactiveValuesToList(Object),"C:/temp/slickobject.rda") # !alert
      #FilterOMs(Object, input, SNkeep, MPkeep, Detkeep, Stochkeep, Projkeep,Det, Stoch, Proj)
      #})

    } else {

    }

  })



  # ---- Download ----
  # not currently used
  # output$downloadData <- downloadHandler(
  #
  #   # replace with Slick$name ...
  #   filename = function() {
  #     paste('Slick-', Sys.Date(), '.slick', sep='')
  #   },
  #
  #   content=function(file) {
  #     if (Object$Loaded) {
  #       saveRDS(Object$obj, file)
  #     } else{
  #       # message no object loaded
  #     }
  #
  #   }
  # )


  # -- Server Modules ----
  # filters
  FiltersServer('filters', Object, SNkeep, MPkeep, Detkeep, Stochkeep, Projkeep,
                Det, Stoch, Proj, i18n = i18n)

  # home
  HomeServer('home', i18n = i18n)

  # load
  LoadServer('load', Object, i18n = i18n)

  # resources
  ResourcesServer('resources')

  # Plots
  # Deterministic
  SpiderServer('spider', Det, MPkeep, Detkeep, SNkeep, Object, window_dims, i18n)

  Spider_OMServer('spiderOM', Det, MPkeep, Detkeep, SNkeep, Object, i18n)

  ZigzagServer('zigzag', Det, MPkeep, Detkeep, SNkeep, Object, window_dims, i18n)

  RailServer('rail', Det, MPkeep, Detkeep, SNkeep, Object, i18n)

  # Stochastic
  BoxplotServer('boxplot', Stoch, MPkeep, Stochkeep, SNkeep, Object, i18n)

  Boxplot_OMServer('boxplotOM', Stoch, MPkeep, Stochkeep, SNkeep, Object, i18n)

  ViolinServer('violin', Stoch, MPkeep, Stochkeep, SNkeep, Object, i18n)

  # Projected
  KobeServer('kobe', Proj, MPkeep, Projkeep, SNkeep, Object, i18n)

  KobeTimeServer('kobetime', Proj, MPkeep, Projkeep, SNkeep, Object, i18n)

  SlopeServer('slope', Proj, MPkeep, Projkeep, SNkeep, Object, i18n)

  # State Variables
  LineServer('line', MPkeep, SNkeep, Object, i18n)

  LineOMServer('lineOM', MPkeep, SNkeep, Object, i18n)

  LineOMSimServer('lineOMSim', MPkeep, SNkeep, Object, i18n)

  # Log ----------------------------------------------------------
  output$Log<-renderText(Log_text$text)

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

}

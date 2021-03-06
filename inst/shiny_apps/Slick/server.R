

options(shiny.maxRequestSize=1000*1024^2)

server <- function(input, output, session) {

  # ---- Initialize Reactive Values -----
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

  window_dims <<- reactive(input$dimension)

  # # Non technical arrays
  Det<-reactiveValues(mat=array())
  Stoch<-reactiveValues(mat=array())
  Proj<-reactiveValues(mat=array())

  # Selections
  SNkeep <- reactiveValues(selected=T)
  MPkeep <- reactiveValues(selected=T)
  Detkeep <- reactiveValues(selected=T)
  Stochkeep <- reactiveValues(selected=T)
  Projkeep <- reactiveValues(selected=T)


  # Log (currently not used)
  Log_text<-reactiveValues(text="nothing happened yet")

  # ---- Observe Events -----
  observeEvent(input$SplashLoad, {
    Object$File <- input$SplashLoad
    Object$Loaded <- Object$Loaded + 1

  })

  observeEvent(input$example_action, {
    Object$File <- input$example_input
    Object$Loaded <- Object$Loaded + 1
  })


  observeEvent(Object$Loaded, {
    if (Object$Loaded >= 1) {
      Object$Ready <- FALSE
      Object$Filt <- FALSE

    # load the object

      if (inherits(Object$File, 'character')) {


        if (Object$File == 'Demonstration') {
          obj <- readRDS("./data/SLICKobj.rda")
        }

        if (Object$File == 'Atlantic bluefin tuna') {
          obj <- readRDS("./data/ABT.slick")
        }

        if (Object$File == 'North Atlantic swordfish') {
          obj <- readRDS("./data/SWO.slick")
        }

      }

      if (inherits(Object$File, 'data.frame')) {
        obj <- readRDS(Object$File$datapath)
        obj$name <- Object$File$name

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
  output$downloadData <- downloadHandler(

    # replace with Slick$name ...
    filename = function() {
      paste('Slick-', Sys.Date(), '.slick', sep='')
    },

    content=function(file) {
      if (Object$Loaded) {
        saveRDS(Object$obj, file)
      } else{
        # message no object loaded
      }

    }
  )

  # filters
  FiltersServer('filters', Object, SNkeep, MPkeep, Detkeep, Stochkeep, Projkeep,
                Det, Stoch, Proj)



  # splash page
  SplashServer('splash', Object)


  # Non technical pages -------------------------------------------
  # page 1
  SpiderServer('spider', Det, MPkeep, Detkeep, SNkeep, Object) # uses modules, all server and ui code contained in Page_1.r

  # page 2
  ZigzagServer('zigzag', Det, MPkeep, Detkeep, SNkeep, Object) # uses modules, all server and ui code contained in Page_2.r

  # page 3
  RailServer('rail', Det, MPkeep, Detkeep, SNkeep, Object)

  # page 4
  KobeServer('kobe', Proj, MPkeep, Projkeep, SNkeep, Object)

  # page 5
  KobeTimeServer('kobetime', Proj, MPkeep, Projkeep, SNkeep, Object)

  # page 6
  LineServer('line', MPkeep, SNkeep, Object)

  # page 7
  SlopeServer('slope', Proj, MPkeep, Projkeep, SNkeep, Object)

  # page 8
  BoxplotServer('boxplot', Stoch, MPkeep, Stochkeep, SNkeep, Object)

  # page 9
  Boxplot_OMServer('boxplotOM', Stoch, MPkeep, Stochkeep, SNkeep, Object)

  # page 10
  Spider_OMServer('spiderOM', Det, MPkeep, Detkeep, SNkeep, Object)

  # page 11
  LineOMServer('lineOM', MPkeep, SNkeep, Object)

  ResourcesServer('resources')


  # Log ----------------------------------------------------------
  output$Log<-renderText(Log_text$text)

  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)


}

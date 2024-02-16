# obj<-readRDS("inst/shiny_apps/SLICK/data/case_studies/SLICKobj.rda")
#
# obj<-readRDS("inst/shiny_apps/SLICK/data/case_studies/NSWO.slick")
# obj <- readRDS('inst/shiny_apps/SLICK/data/case_studies/WSKJ.slick')
#
#
#





Object <- list()
Object$obj <- obj

nFac <- ncol(obj$OM$Design)
nsim<-dim(obj$Perf$Stoch$Values)[1]

nSN <- nrow(obj$OM$Design)
nMP <- length(obj$MP$Labels)
nPMd <- dim(obj$Perf$Det$Values)[3]
nPMs <- dim(obj$Perf$Stoch$Values)[4]
nPMp <- dim(obj$Perf$Proj$Values)[4]

SNkeep <- NULL; SNkeep$selected=rep(T,nSN)
MPkeep <- NULL; MPkeep$selected=rep(T,nMP)
Detkeep <- NULL; Detkeep$selected=rep(T,nPMd)
Stochkeep <- NULL; Stochkeep$selected=rep(T,nPMs)
Projkeep <- NULL;  Projkeep$selected=rep(T,nPMp)


Det<- NULL; Det$mat <- obj$Perf$Det$Values
Proj <- NULL; Proj$mat <- obj$Perf$Proj$Values
Stoch <- NULL; Stoch$mat <- obj$Perf$Stoch$Values

input <- NULL; input$SV_select <- 3
input$selectSV <- obj$StateVar$Labels[input$SV_select]
input$OM_select <- 4


Det$mat %>% dim
Stoch$mat %>% dim

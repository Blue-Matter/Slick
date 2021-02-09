

#' Creates a blank Slick object
#'
#' A function that creates a blank Slick object. The key requirement is that the dimensions of the analysis are known including the Design matrix
#'
#' @param name Character string that is the object name (shortened for use in menus etc.)
#' @param nPerf An integer vector of the number of deterministic (nD), stochastic (nS) and projected (nP) performance metrics
#' @param nMPs Integer, the number of management options (aka management procedures).
#' @param nsim Integer, the number of simulations (stochastic replicates per state of nature)
#' @param nProjYr Integer, the number of projected years
#' @param nStateVar Integer, the number of state variables
#' @param nHistYr Integer, the number of historical years for state variables
#' @param Design A design matrix of factor levels [SN, factor]
#'
#' @return An object of class \linkS4class{Slick}
#' @author T. Carruthers
#' @export

NewSlick<-function(name = "Unnamed Slick Object", nPerf=list(nD=5, nS=6, nP=7), nMPs=5, nsim=10, nProjYr=50, nStateVar=2, nHistYr=55, Design=expand.grid(1:2, 1:2)){

  out<-list()
  nOM<-nrow(Design)  # number of OMs (MSE runs)
  nFac<-ncol(Design) # number of design factors

  out$name<-name

  # === Performance results ===============================================================================================
  out$Perf<-list()

    # --- Deterministic results ------------------------------------------------------------------------
    out$Perf$Det<-list()

      out$Perf$Det$Labels<-paste("Deterministic PM",1:nPerf$nD)
      out$Perf$Det$Codes<-paste0("Det_PM_",1:nPerf$nD)
      out$Perf$Det$Description<-paste0("About deterministic performance metric",1:nPerf$nD)
      out$Perf$Det$Values<-array(NA,c(nOM,nMPs,nPerf$nD))
      out$Perf$Det$RefPoints<-list()
      out$Perf$Det$RefNames<-as.list(paste("DetRefName",1:nPerf$nD))

    # --- Stochastic results ---------------------------------------------------------------------------
    out$Perf$Stoch<-list()

      out$Perf$Stoch$Labels<-paste("Stochastic PM",1:nPerf$nS)
      out$Perf$Stoch$Codes<-paste0("Stoch_PM_",1:nPerf$nS)
      out$Perf$Stoch$Description<-paste0("About stochastic performance metric",1:nPerf$nS)
      out$Perf$Stoch$Values<-array(NA,c(nsim,nOM,nMPs,nPerf$nS))
      out$Perf$Stoch$RefPoints<-list()
      out$Perf$Stoch$RefNames<-as.list(paste("StochRefName",1:nPerf$nS))

    # --- Projected results ---------------------------------------------------------------------------
    out$Perf$Proj<-list()

      out$Perf$Proj$Labels<-paste("Projected PM",1:nPerf$nP)
      out$Perf$Proj$Codes<-paste0("Proj_PM_",1:nPerf$nP)
      out$Perf$Proj$Description<-paste0("About projected performance metric",1:nPerf$nS)
      out$Perf$Proj$Values<-array(NA,c(nsim,nOM,nMPs,nPerf$nP,nProjYr))
      out$Perf$Proj$Times<-as.integer(format(Sys.Date(), "%Y"))+(1:nProjYr)
      out$Perf$Proj$RefPoints<-list()
      out$Perf$Proj$RefNames<-as.list(paste("ProjRefName",1:nPerf$nP))

  # === States of nature list (aka operating model) =========================================================================
  out$OM<-list()

    out$OM$Design<-Design # defaults to four OMs across 2 factors
    out$OM$Factor_Labels<-paste("Factor",1:nFac)
    out$OM$Labels<-out$OM$Codes<-out$OM$Description<-list()

    for(fac in 1:nFac){
      out$OM$Labels[[fac]]<-paste("Factor",fac,"level",unique(out$OM$Design[,fac]))
      out$OM$Codes[[fac]]<-paste0("F",fac,"L",unique(out$OM$Design[,fac]))
      out$OM$Description[[fac]]<-paste("Factor", fac, "level",unique(out$OM$Design[,fac]),"means this and that")
    }

  # === State Variable list (e.g. population biomass)

  out$StateVar<-list()

    out$StateVar$Labels<-paste("State Variable",1:nStateVar)
    out$StateVar$Codes<-paste0("SV_",1:nStateVar)
    out$StateVar$Description<-paste0("About state variable",1:nStateVar)
    out$StateVar$Values<-array(NA,c(nsim,nOM,nMPs,nStateVar,nProjYr+nHistYr))
    out$StateVar$Times<-as.integer(format(Sys.Date(), "%Y"))+(-(nHistYr-1):nProjYr)
    out$StateVar$RefPoints<-list()
    out$StateVar$RefNames<-as.list(paste("StateVarRefName",1:nStateVar))
    out$StateVar$TimeNow<-out$StateVar$Times[nHistYr]

  # === Management options (aka management procedures) ======================================================================
  out$MP<-list()

    out$MP$Labels<-paste("Management Procedures",1:nMPs)
    out$MP$Codes<-paste0("MO",1:nMPs)
    out$MP$Description<-paste("Management Procedure",1:nMPs,"operates in such and such a way")

  # === App Text  ===========================================================================================================
  out$Text<-list()

    out$Text$Title<-"Slick"
    out$Text$Sub_title<-"Decision Analysis"
    out$Text$Introduction<-list("","","")

  # === Misc ================================================================================================================
  out$Misc<-list()

      tc<-function(r,g,b)rgb(r,g,b,maxColorValue=255)
      out$Misc$Author="Anon"
      out$Misc$Contact=NA
      out$Misc$Date=Sys.time()
      out$Misc$Institution=NA
      out$Misc$Logo=NA
      out$Misc$App_axes<-c(PM="Performance metric", SN="Operating model", MO="Management Procedure", Sim="Simulation",Time="Year")
      out$Misc$App_axes_code<-c(PM="PM",SN="OM",MO="MP",Sim="Sim",Time="Yr")
      out$Misc$Cols<-list(MP=c(tc(0,120,58),tc(144,189,55),tc(178,179,183),tc(247,149,29),tc(207,60,37),tc(254,202,10)),
                 BG=c(main='white',box=tc(228,233,237),spider=tc(225,226,228)),
                 KobeBG=c(R=tc(216,119,93),OFing=tc(248,220,122),OF=tc(253,189,86),G=tc(103,193,139)),
                 Kobeline='white',
                 KobeText=c(R=tc(138,0,60),OFing=tc(152,137,3),OF=tc(144,102,0),G=tc(1,144,70)),
                 KobePoint=c(R=tc(189,16,24),OFing=tc(234,107,31),OF=tc(234,107,31),G=tc(0,123,59)),
                 RefPt=c(target=tc(3,165,79),limit=tc(238,29,35),zeroC=tc(147,182,217)))

  class(out)<-"Slick"
  out

}

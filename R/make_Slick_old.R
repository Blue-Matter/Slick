#' Make an example Slick object
#'
#' A function that creates an example Slick data object
#'
#' @param name Character string that is the object name (abbreviated for use in menus etc)
#' @param OM An operating model object (class 'OM')
#' @param MPs A vector of methods (character string) of class MP
#' @param MP_Desc A vector method descriptions (character string) nMPs long
#' @param PMs A vector of performance metrics of class PM
#' @param Design A design matrix of OM runs
#' @param SN A list of Labels, Codes and Descriptions of the factor levels. Each list item is a factor containing a vector of factor levels.
#' @param mods A nested list of mods
#' @param nsim Integer, the number of simulations
#' @param MSElist An optional list of prerun MSEs
#' @param fstYr An optional numeric value for first projection year. Otherwise current year is used
#' @param returnMSEs Logical, rather than the Slick object should the list of MSEs be returned?
#'
#' @return An object of class [Slick-class]
#' @author T. Carruthers
#' @export
Make_Slick<-function(name = "Unnamed Slick object",
                     OM=NULL,
                     MPs=c("DCAC", "AvC", "Fratio", "FMSYref", "FMSYref50","matlenlim"),
                     MP_Desc = NULL,
                     PMs=c("AAVE","AAVY","LTY","P10","P50","P100","PNOF","STY","Yield"),
                     Design=as.data.frame(cbind(rbind(as.matrix(expand.grid(1:2,1:3,1:2)),matrix(1,nrow=5,ncol=3)),c(rep(1,12),2:6))),
                     SN=list(Factor_Labels=c("Natural Mortality","Resilience","Stock Depletion","Robustness"),
                             Labels=list(c("M=0.2","M=0.3"),c("h=0.5","h=0.7","h=0.9"),c("Dep=0.1","Dep=0.3"),c("Ref_Case","L50=0.5","Vmaxlen=0.1","Cobs=0.5","Perr=0.5","AC=0.95")),
                             Codes=list(c("M2","M3"),c("h5","h7","h9"),c("D1","D3"),c("Ref_case","mat_low","dome","h_Cerr","h_Perr","h_AC")),
                             Description=list(c("M=0.2","M=0.3"),c("h=0.5","h=0.7","h=0.9"),c("Dep=0.1","Dep=0.3"),c("Reference Case","L50=0.5","Vmaxlen=0.1","Cobs=0.5","Perr=0.5","AC=0.95"))),
                     mods=list(function(OM,lev){if(lev==1)OM@M<-c(0.2,0.2);if(lev==2)OM@M<-c(0.3,0.3);OM},  # M
                               function(OM,lev){if(lev==1)OM@h<-c(0.5,0.5);if(lev==2)OM@h=c(0.7,0.7);if(lev==3)OM@h=c(0.9,0.9);OM},# steepness
                               function(OM,lev){if(lev==1)OM@D<-c(0.1,0.1);if(lev==2)OM@D<-c(0.3,0.3);OM}, # current depletion
                               function(OM,lev){if(lev==2)OM@L50<-c(0.5,0.5);if(lev==3)OM@Vmaxlen=c(0.1,0.1);if(lev==4)OM@Cobs=c(0.5,0.5); # robustness OMs
                               if(lev==5)OM@Perr=c(0.5,0.5); if(lev==6)OM@AC=c(0.9,0.9);OM}),
                     nsim=48,
                     MSElist=NULL,
                     fstYr=NULL,
                     returnMSEs=F
){

  .Deprecated("Slick")
  if (!requireNamespace('MSEtool', quietly = TRUE))
    stop('Package `MSEtool` required for this function')

  if (is.null(OM))
    OM <- MSEtool::testOM
  # --- Get dims and create new Slick object -------------------------------------------------------------------------
  runMSEs<-is.null(MSElist)
  if(runMSEs){
    OM@nsim <- nsim
    proyears<-OM@proyears

  }else{
    nsim <- MSElist[[1]]@nsim
    proyears<-MSElist[[1]]@proyears
    nyears<-MSElist[[1]]@nyears

  }

  # Design<-as.data.frame(Design)
  nOM<-nrow(Design)
  nFacs<-ncol(Design)
  nPMs<-length(PMs)
  nMPs<-length(MPs)

  out<-NewSlick(name=name, nPerf=list(nD=nPMs,nS=nPMs,nP=4), nMPs=nMPs, nsim=nsim, nProjYr=proyears, nStateVar=2, nHistYr=nyears, Design=Design)

  # --- Do states of nature labelling (matches levels of the Design grid) ---------------------------------------------
  out$OM$Factor_Labels <- SN$Factor_Labels
  out$OM$Labels <- SN$Labels
  out$OM$Codes <- SN$Codes
  out$OM$Description <- SN$Description

  out$MP$Labels <- MPs
  out$MP$Codes <- MPs
  if(is.null(MP_Desc)){
    out$MP$Description<-c("TAC is Depletion-Corrected Average Catch (MacCall 2009)","TAC is Average historical catches",
                          "TAC is a fixed ratio of FMSY/M multiplied by M, multiplied by surveyed biomass","TAC is perfectly known FMSY fishing",
                          "TAC is half of perfectly known FMSY fishing","No TAC restriction, only a minimum size limit at length at maturity")
  }else{

    out$MP$Description <- MP_Desc
  }

  # --- Do performance metric labelling -------------------------------------------------------------------------------

  # Deterministic and Stochastic are the same PMs here and therefore of equal length
  out$Perf$Det$Codes<-out$Perf$Stoch$Codes<-PMs # Codes are easy
  if(is.null(MSElist)){
    OMtemp<-OM
    OMtemp@nsim=4
    MSEtemp<-MSEtool::runMSE(OMtemp,MPs='AvC')
  }else{
    MSEtemp<-MSElist[[1]]
  }
  for(p in 1:nPMs){
    fun <- get(PMs[p])
    PMtemp<-  fun(MSEtemp)
    out$Perf$Det$Labels[p]<-out$Perf$Stoch$Labels[p]<-PMtemp@Caption
    out$Perf$Det$Description[p]<-out$Perf$Stoch$Description[p]<-PMtemp@Name
  }

  out$Perf$Det$RefPoints <- out$Perf$Det$RefNames <- out$Perf$Stoch$RefPoints <- out$Perf$Stoch$RefNames <- rep(list(NA),each=nPMs)

  out$Perf$Proj$Labels <- out$Perf$Proj$Codes <- c("SSB/SSBMSY","F/FMSY","SSB/SSB0","Catch/MSY")
  out$Perf$Proj$Description=c("Spawning stock biomass relative to MSY levels", "Apical fishing mortality rate relative to MSY levels",
                              "Spawning stock biomass relative to unfished levels","Catch relative to MSY")
  if (is.null(fstYr)) fstYr <- as.integer(format(Sys.Date(), "%Y"))
  out$Perf$Proj$Times<-seq(fstYr, by=1,length.out=proyears)
  out$Perf$Proj$Time_lab<-"Year"
  out$Perf$Proj$RefPoints<-list(c(0.5,1),c(1.4,1),c(0.2,0.4),c(1.4,1))
  out$Perf$Proj$RefNames<-rep(list(c('Limit','Target')),each=4)


  # --- Do State Variable labelling -----------------------------------------------------------------------------------

  out$StateVar$Labels <- c("Spawning Stock Biomass", "Spawning Stock Biomass relative to MSY levels",
                           'Yield')
  out$StateVar$Codes <- c("SSB", "SSB_SSBMSY", 'Yield')
  out$StateVar$Description <- c("Spawning Stock Biomass","Spawning Stock Biomass relative to MSY levels", 'Yield')
  out$StateVar$Time_lab <- "Year"
  out$StateVar$RefPoints <- list(NA,c(1,0.5))
  out$StateVar$RefNames <- list(NA, c("Target","Limit"))
  out$StateVar$TimeNow<-as.integer(format(Sys.Date(), "%Y"))
  out$StateVar$Values<-array(NA,c(nsim,nOM,nMPs,3,nyears + proyears))
  out$StateVar$Times<- c(rev(seq(fstYr-1, by=-1,length.out=nyears)),
                         seq(fstYr, by=1,length.out=proyears))

  # --- Run MSEs ------------------------------------------------------------------------------------------------------

  if(runMSEs){
    MSElist=list()
    MSEtool::setup()
  }

  for(i in 1:nOM){

    if(runMSEs){
      OMtemp<-OM
      for(Fac in 1:nFacs)  OMtemp<-mods[[Fac]](OMtemp,Design[i,Fac])
      MSElist[[i]]<-MSEtool::runMSE(OMtemp,MPs=MPs,parallel=T)
    }

    MSEtemp<-MSElist[[i]]

    for(p in 1:nPMs){
      fun <- get(PMs[p])
      PMtemp<-  fun(MSEtemp)

      convsims<-1:(dim(PMtemp@Prob)[1])
      #dim(out$Perf$Det$Values)
      out$Perf$Det$Values[i,,p]<-PMtemp@Mean*100
      #dim(out$Perf$Stoch$Values)
      out$Perf$Stoch$Values[convsims,i,,p]<-PMtemp@Prob*100
    }

    #(B/BMSY, F/FMSY, SSB/SSB0, Catch/MSY)
    if (inherits(MSEtemp, 'MMSE')) {
      out$Perf$Proj$Values[convsims,i,,1,]<-MSEtemp@SB_SBMSY[,1,,] # female - assumed first stock
      out$Perf$Proj$Values[convsims,i,,2,]<-MSEtemp@F_FMSY[,1,1,,] # female - assumed first stock - and first fleet
      out$Perf$Proj$Values[convsims,i,,3,]<-MSEtemp@SSB[,1,,]/(MSEtemp@RefPoint$ByYear$SSB0[,1,,nyears:(nyears+proyears-1)])
      out$Perf$Proj$Values[convsims,i,,4,]<-apply(MSEtemp@Catch, c(1,4,5), sum)/apply(MSEtemp@RefPoint$ByYear$MSY[,,,(nyears):(nyears+proyears-1)], c(1,3,4), sum)

      SSB_hist <- apply(MSEtemp@multiHist[[1]][[1]]@TSdata$SBiomass, 1:2, sum)
      SSB_proj <- MSEtemp@SSB[,1,,]
      SSBMSY <- MSEtemp@RefPoint$ByYear$SSBMSY[,1,,]
      for(mp in 1:nMPs)    out$StateVar$Values[convsims,i,mp,1,1:MSEtemp@nyears]<-SSB_hist # SSB (lazy loop over MPs)
      out$StateVar$Values[convsims,i,,1,MSEtemp@nyears+(1:MSEtemp@proyears)]<-SSB_proj # SSB
      out$StateVar$Values[convsims,i,,2,]<-out$StateVar$Values[convsims,i,,1,]/SSBMSY   # non-time varying SSB relative to SSBMSY (nyear)

    } else {
      out$Perf$Proj$Values[convsims,i,,1,]<-MSEtemp@SB_SBMSY
      out$Perf$Proj$Values[convsims,i,,2,]<-MSEtemp@F_FMSY
      out$Perf$Proj$Values[convsims,i,,3,]<-MSEtemp@SSB/MSEtemp@OM$SSB0
      out$Perf$Proj$Values[convsims,i,,4,]<-MSEtemp@Catch/MSEtemp@OM$RefY

      for(mp in 1:nMPs)    out$StateVar$Values[convsims,i,mp,1,1:MSEtemp@nyears]<-MSEtemp@SSB_hist # SSB (lazy loop over MPs)
      out$StateVar$Values[convsims,i,,1,MSEtemp@nyears+(1:MSEtemp@proyears)]<-MSEtemp@SSB  # SSB
      out$StateVar$Values[convsims,i,,2,]<-out$StateVar$Values[convsims,i,,1,]/MSEtemp@OM$SSBMSY   # non-time varying SSB relative to SSBMSY (nyear)

      # add Yield
      for(mp in 1:nMPs) {
        out$StateVar$Values[convsims,i,mp,3,1:MSEtemp@nyears] <- MSEtemp@CB_hist
      }

      out$StateVar$Values[convsims,i,,3,MSEtemp@nyears+(1:MSEtemp@proyears)] <- MSEtemp@Catch
    }
    print(paste(i,"of",nOM,"states of nature completed"))
  }

  #for(p in 1:nPMs){ # rescaling
  #out$Perf$Det$Values[,,p]<-out$Perf$Det$Values[,,p]/max(out$Perf$Det$Values[,,p])*100
  #out$Perf$Stoch$Values[,,,p]<-out$Perf$Stoch$Values[,,,p]/max(out$Perf$Stoch$Values[,,,p])*100
  #}
  colnames(out$OM$Design)<- SN$Factor_Labels

  out$TimeNow=2021

  #saveRDS(MSElist,"C:/temp/MSElist.rda")
  if(returnMSEs){
    return(list(MSElist,out))
  }else{
    return(out)
  }

}

#' Creates a blank Slick object
#'
#'
#' @param name Character string that is the object name (shortened for use in menus etc.)
#' @param nPerf An integer vector of the number of deterministic (nD), stochastic (nS) and projected (nP) performance metrics
#' @param nMPs Integer, the number of management options (aka management procedures).
#' @param nsim Integer, the number of simulations (stochastic replicates per state of nature)
#' @param nProjYr Integer, the number of projected years
#' @param nStateVar Integer, the number of state variables
#' @param nHistYr Integer, the number of historical years for state variables
#' @param Design A design matrix of factor levels `SN, factor`
#'
#' @return An object of class [Slick-class]
#' @author T. Carruthers
#' @export

NewSlick<-function(name = "Unnamed Slick Object", nPerf=list(nD=5, nS=6, nP=7), nMPs=5, nsim=10, nProjYr=50, nStateVar=2, nHistYr=55, Design=expand.grid(1:2, 1:2)){
  .Deprecated("Slick")
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

  tc<-function(r,g,b)grDevices::rgb(r,g,b,maxColorValue=255)
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

  class(out)<-"Slick_old"
  out

}

# Slick checking

check_Slick<-function(obj){

  err<-list()
  # Performance metrics go from 0 to 100
  # Dims are right (e.g.)
  # Det$Values vs Codes
  # OM$Codes is a list nfac long
  # OM$Description is a list nFac long
  # duplication of factor levels (e.g. non unique codes and descriptions)

  err$Design1<-!all.equal(as.vector(obj$OM$Design), as.integer(as.vector(obj$OM$Design)))
  err$Perfd<-!all(obj$Perf$Det$Values>=0 & obj$Perf$Det$Values<=100)
  err$Perfs<-!all(obj$Perf$Stoch$Values>=0 & obj$Perf$Stoch$Values<=100)
  err
}

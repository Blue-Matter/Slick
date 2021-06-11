

#' Make an example Slick object
#'
#' A function that creates an example Slick data object
#'
#' @param name Character string that is the object name (abbreviated for use in menus etc)
#' @param OM An operating model object (class 'OM')
#' @param MPs A vector of methods (character string) of class MP
#' @param PMs A vector of performance metrics of class PM
#' @param Design A design matrix of OM runs [run, mod]
#' @param SN A list of Labels, Codes and Descriptions of the factor levels. Each list item is a factor containing a vector of factor levels.
#' @param mods A nested list of mods [mod type long]
#' @param nsim Integer, the number of simulations
#' @param MSElist An optional list of prerun MSEs
#' @param returnMSEs Logical, rather than the Slick object should the list of MSEs be returned?
#'
#' @return An object of class \linkS4class{Slick}
#' @author T. Carruthers
#' @export
Make_Slick<-function(name = "Unnamed Slick object",
                     OM=testOM,
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
                     returnMSEs=F
                     ){

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
  OMtemp<-OM
  OMtemp@nsim=4
  MSEtemp<-runMSE(OMtemp,MPs='AvC')
  for(p in 1:nPMs){
    PMtemp<-do.call(PMs[p],args=list(MSEobj=MSEtemp))
    out$Perf$Det$Labels[p]<-out$Perf$Stoch$Labels[p]<-PMtemp@Caption
    out$Perf$Det$Description[p]<-out$Perf$Stoch$Description[p]<-PMtemp@Name
  }

  out$Perf$Det$RefPoints <- out$Perf$Det$RefNames <- out$Perf$Stoch$RefPoints <- out$Perf$Stoch$RefNames <- rep(list(NA),each=nPMs)

  out$Perf$Proj$Labels <- out$Perf$Proj$Codes <- c("SSB/SSBMSY","F/FMSY","SSB/SSB0","Catch/MSY")
  out$Perf$Proj$Description=c("Spawning stock biomass relative to MSY levels", "Apical fishing mortality rate relative to MSY levels",
                              "Spawning stock biomass relative to unfished levels","Catch relative to MSY")
  out$Perf$Proj$Times<-as.integer(format(Sys.Date(), "%Y"))+(1:OM@proyears)
  out$Perf$Proj$Time_lab<-"Year"
  out$Perf$Proj$RefPoints<-list(c(0.5,1),c(1.4,1),c(0.2,0.4),c(1.4,1))
  out$Perf$Proj$RefNames<-rep(list(c('Limit','Target')),each=4)


  # --- Do State Variable labelling -----------------------------------------------------------------------------------

  out$StateVar$Labels <- c("Spawning Stock Biomass","Spawning Stock Biomass relatve to MSY levels")
  out$StateVar$Codes <- c("SSB", "SSB_SSBMSY")
  out$StateVar$Description <- c("Spawning Stock Biomass","Spawning Stock Biomass relative to MSY levels")
  out$StateVar$Times<-as.integer(format(Sys.Date(), "%Y"))+(-(OM@nyears-1):OM@proyears)
  out$StateVar$Time_lab <- "Year"
  out$StateVar$RefPoints <- list(NA,c(1,0.5))
  out$StateVar$RefNames <- list(NA, c("Target","Limit"))
  out$StateVar$TimeNow<-as.integer(format(Sys.Date(), "%Y"))
  out$StateVar$Values<-array(NA,c(nsim,nOM,nMPs,2,nyears + proyears))

  # --- Run MSEs ------------------------------------------------------------------------------------------------------

  if(runMSEs){
    MSElist=list()
    setup()
  }

  for(i in 1:nOM){

    if(runMSEs){
      OMtemp<-OM
      for(Fac in 1:nFacs)  OMtemp<-mods[[Fac]](OMtemp,Design[i,Fac])
      MSElist[[i]]<-runMSE(OMtemp,MPs=MPs,parallel=T)
    }

    MSEtemp<-MSElist[[i]]

    for(p in 1:nPMs){

       PMtemp<-do.call(PMs[p],args=list(MSEobj=MSEtemp))
       convsims<-1:(dim(PMtemp@Prob)[1])
       #dim(out$Perf$Det$Values)
       out$Perf$Det$Values[i,,p]<-PMtemp@Mean*100
       #dim(out$Perf$Stoch$Values)
       out$Perf$Stoch$Values[convsims,i,,p]<-PMtemp@Prob*100

    }
    #(B/BMSY, F/FMSY, SSB/SSB0, Catch/MSY)
    out$Perf$Proj$Values[convsims,i,,1,]<-MSEtemp@SB_SBMSY
    out$Perf$Proj$Values[convsims,i,,2,]<-MSEtemp@F_FMSY
    out$Perf$Proj$Values[convsims,i,,3,]<-MSEtemp@SSB/MSEtemp@OM$SSB0
    out$Perf$Proj$Values[convsims,i,,4,]<-MSEtemp@Catch/MSEtemp@OM$RefY

    for(mp in 1:nMPs)    out$StateVar$Values[convsims,i,mp,1,1:MSEtemp@nyears]<-MSEtemp@SSB_hist # SSB (lazy loop over MPs)
    out$StateVar$Values[convsims,i,,1,MSEtemp@nyears+(1:MSEtemp@proyears)]<-MSEtemp@SSB  # SSB
    out$StateVar$Values[convsims,,,2,]<-out$StateVar$Values[convsims,,,1,]/MSEtemp@OM$SSBMSY   # non-time varying SSB relative to SSBMSY (nyear)

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






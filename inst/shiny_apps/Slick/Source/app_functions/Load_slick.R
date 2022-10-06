

Update_dims<-function(){

  # Dimensions
  nFac <<- ncol(Objectobj$OM$Design)
  nsim <<-dim(obj$Perf$Stoch$Values)[1]
  nSN <<- nrow(obj$OM$Design)
  nMP <<- length(obj$MP$Labels)
  nPMd <<- dim(obj$Perf$Det$Values)[3]
  nPMs <<- dim(obj$Perf$Stoch$Values)[4]
  nPMp <<- dim(obj$Perf$Proj$Values)[4]

}

Refresh_UI<-function(){

  Update_dims()
  # States of Nature Filters

    lapply(1:4, function(i) { # should be nFac
      updateCheckboxGroupInput(paste0("Fil_SN_1_",i),inline=T,obj$OM$Factor_Labels[i],choiceNames=obj$OM$Labels[[i]],selected=1:length(obj$OM$Labels[[i]]),choiceValues=1:length(obj$OM$Labels[[i]]))
    })

  # States of Nature Filters



  #lapply(1:nFac, function(i) {
   #   updatecheckboxGroupInput(paste0("Fil_SN_2_",i),inline=T,obj$OM$Factor_Labels[i],choiceNames=obj$OM$Labels[[i]],selected=1:length(obj$OM$Labels[[i]]),choiceValues=1:length(obj$OM$Labels[[i]]))
  #  })


  # MP filtering

    updateCheckboxGroupInput("Fil_MP_T",label=NULL,inline=T,choices=obj$MP$Labels,selected=obj$MP$Labels)


  # Deterministic Performance metric filtering

    updateCheckboxGroupInput("Fil_PM_Det_T",label="Deterministic",inline=T,choices=obj$Perf$Det$Codes,selected=obj$Perf$Det$Codes)


  # Stochastic Performance metric filtering

    updateCheckboxGroupInput("Fil_PM_Stoch_T",label="Stochastic",inline=T,choices=obj$Perf$Stoch$Codes,selected=obj$Perf$Stoch$Codes)


  # Projection performance metric filtering

    updateCheckboxGroupInput("Fil_PM_Proj_T",label="Projected",inline=T,choices=obj$Perf$Proj$Codes,selected=obj$Perf$Proj$Codes)



}

#' Normalize performance metric values in a Slick object
#'
#' A function that converts deterministic or stochastic performance metrics to the range 0-100 and optionally inverts these
#'
#' @param obj An object of class 'slick'
#' @param det logical, should the normalization be applied to the deterministic performance metrics (or, if false the stochastic ones)
#' @param inv A logical vector nPM long. If true, the PM will be inverted (100-value).
#'
#' @return An object of class [Slick-class]
#' @author T. Carruthers
#' @export
PMnorm<-function(obj,det=TRUE,inv=NULL){

  zero100<-function(x)((x-min(x))/(max(x)-min(x)))*100
  if(det){
    nPMs<-length(obj$Perf$Det$Labels)
    for(i in 1:nPMs){
      obj$Perf$Det$Values[,,i]<-zero100(obj$Perf$Det$Values[,,i])
      if(!is.null(inv))if(inv[i])obj$Perf$Det$Values[,,i]<-100-obj$Perf$Det$Values[,,i]
    }
  }else{
    nPMs<-length(obj$Perf$Stoch$Labels)
    for(i in 1:nPMs){
      obj$Perf$Stoch$Values[,,,i]<-zero100(obj$Perf$Stoch$Values[,,,i])
      if(!is.null(inv))if(inv[i])obj$Perf$Stoch$Values[,,,i]<-100-obj$Perf$Stoch$Values[,,,i]
    }
  }

  obj

}

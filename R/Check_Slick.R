

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

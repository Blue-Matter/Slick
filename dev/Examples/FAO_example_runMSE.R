# Simple example with three MPs

library(openMSE)
library(Slick)

Stock <- MSEtool::Albacore
Fleet <- MSEtool::Generic_IncE
Obs <- MSEtool::Precise_Unbiased
Imp <- MSEtool::Perfect_Imp



nsim <- 50

OM <- new('OM', Stock, Fleet, Obs, Imp, nsim=nsim)

OM@h <- c(0.7,0.7)

OM@proyears <- 20
OM@interval <- 2

OM@LFS <- c(0.6,0.7)
OM@LFS <- c(0.75, 0.85)
OM@Vmaxlen <- c(1,1)
OM@D <- c(0.3, 0.4)

all_MPs <- avail('MP')
all_MPs <- all_MPs[!grepl('ref', all_MPs)]
all_MPs <- all_MPs[!grepl('DDSS', all_MPs)]
all_MPs <- all_MPs[!grepl('SCA', all_MPs)]
all_MPs <- all_MPs[!grepl('SSS', all_MPs)]
all_MPs <- all_MPs[!grepl('Lratio', all_MPs)]

Hist <- Simulate(OM, silent=TRUE)
saveRDS(Hist, 'dev/Examples/Example_1.hist')

Hist <- readRDS('dev/Examples/Example_1.hist')

MSE_all <- Project(Hist, MPs=all_MPs, parallel = TRUE)

saveRDS(MSE_all, 'dev/Examples/Example_1.mse')

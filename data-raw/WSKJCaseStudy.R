# pak::pak('Slick')

library(Slick)
library(ggplot2)
library(patchwork)

WSKJ <- download_casestudy('Western Atlantic Skipjack')


# ---- Add Boxplot ----

dd   <- dim(WSKJ@Timeseries@Value) # nsim, nOM, nMP, nPI, nTS
nSim <- dd[1]
nOM  <- dd[2]
nMP  <- dd[3]

proj_ind <- 75:104

WSKJ@Boxplot@Code <- WSKJ@Timeseries@Code[1:3]
WSKJ@Boxplot@Label <- WSKJ@Timeseries@Label[1:3]
WSKJ@Boxplot@Value <- array(NA, dim=c(nSim, nOM, nMP, 3))

for (om in seq_len(nOM)) {
  for (mp in seq_len(nMP)) {
    WSKJ@Boxplot@Value[,om, mp, 1] <- apply(WSKJ@Timeseries@Value[,om, mp, 1,proj_ind], 1, mean)
    WSKJ@Boxplot@Value[,om, mp, 2] <- apply(WSKJ@Timeseries@Value[,om, mp, 2,proj_ind], 1, mean)
    WSKJ@Boxplot@Value[,om, mp, 3] <- apply(WSKJ@Timeseries@Value[,om, mp, 3,proj_ind], 1, mean)
  }
}

# ---- Add Quilt ----

quilt <- Quilt(Code=c('PGK',
                      'P100',
                      'PNOF',
                      'Mean TAC'),
               Label=c('Prob. Green Kobe',
                       'Prob SB>SBMSY',
                       'Prob. Not Overfishing',
                       'Mean Total Allowable Catch'),
               Description = c('Probability of being in the green region of Kobe plot over the projection period',
                               'Probability spawning biomass is greater than SB_MSY over the projection period',
                               'Probability of not overfishing over the projection period',
                               'Mean Total Allowable Catch over the projection period'))

Value(quilt) <- array(NA, dim=c(nOM, nMP, 4))

for (om in seq_len(nOM)) {
  for (mp in seq_len(nMP)) {

    quilt@Value[om, mp, 1] <- mean(WSKJ@Timeseries@Value[,om,mp,1,proj_ind] > 1 &
                                     WSKJ@Timeseries@Value[,om,mp,2,proj_ind] < 1)
    quilt@Value[om, mp, 2] <- mean(WSKJ@Timeseries@Value[,om,mp,1,proj_ind] > 1)
    quilt@Value[om, mp, 3] <- mean(WSKJ@Timeseries@Value[,om,mp,2,proj_ind] < 1)
    quilt@Value[om, mp, 4] <- mean(WSKJ@Timeseries@Value[,om,mp,3,proj_ind])
  }

}

Quilt(WSKJ) <- quilt

# ---- Add Spider ----

spider <- Spider(Code=c('P100',
                        'P50',
                        'PNOF'),
                 Label=c('Prob SB>SBMSY',
                         'Prob SB>0.5SBMSY',
                         'Prob. Not Overfishing'),
                 Description = c('Probability spawning biomass is greater than SB_MSY over the projection period',
                                 'Probability spawning biomass is greater than 0.5 SB_MSY over the projection period',
                                 'Probability of not overfishing over the projection period'
                 ))


nPI <- length(Code(spider))

Value(spider) <- array(NA, dim=c(1, nMP, nPI))

for (om in 4 ){ #seq_len(nOM)) {
  for (mp in seq_len(nMP)) {

    Value(spider)[1,mp,1] <- mean(WSKJ@Timeseries@Value[,om,mp,1,proj_ind] > 1)
    Value(spider)[1,mp,2] <- mean(WSKJ@Timeseries@Value[,om,mp,1,proj_ind] > 0.5)
    Value(spider)[1,mp,3] <- mean(WSKJ@Timeseries@Value[,om,mp,2,proj_ind] < 1)
  }

}

Spider(WSKJ) <- spider

# ---- Add Trade Off ----

tradeoff <- Tradeoff(Code=c('P100',
                            'P50',
                            'PNOF',
                            'Mean Yield'),
                     Label=c('Prob SB>SBMSY',
                             'Prob SB>0.5SBMSY',
                             'Prob. Not Overfishing',
                             'Mean Yield'),
                     Description = c('Probability spawning biomass is greater than SB_MSY over the projection period',
                                     'Probability spawning biomass is greater than 0.5 SB_MSY over the projection period',
                                     'Probability of not overfishing over the projection period',
                                     ''))


nPI <- length(Code(tradeoff))

Value(tradeoff) <- array(NA, dim=c(nOM, nMP, nPI))

for (om in seq_len(nOM)) {
  for (mp in seq_len(nMP)) {

    Value(tradeoff)[om,mp,1] <- mean(WSKJ@Timeseries@Value[,om,mp,1,proj_ind] > 1)
    Value(tradeoff)[om,mp,2] <- mean(WSKJ@Timeseries@Value[,om,mp,1,proj_ind] > 0.5)
    Value(tradeoff)[om,mp,3] <- mean(WSKJ@Timeseries@Value[,om,mp,2,proj_ind] < 1)
    Value(tradeoff)[om,mp,4] <- mean(WSKJ@Timeseries@Value[,om,mp,3,proj_ind])
  }

}

Tradeoff(WSKJ) <- tradeoff

# ---- Run App ----
# App(slick = WSKJ)


# ---- Save object to package ----
usethis::use_data(WSKJ, overwrite = TRUE)

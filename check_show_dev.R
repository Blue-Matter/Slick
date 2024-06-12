la <- devtools::load_all

la()
# Check and show

# MPs ----

## Empty ----
MPs <- MPs()
MPs

Check(MPs)

## Minimum Complete ----
nMP <- 4
Code(MPs) <- paste('MP', 1:nMP)
Label(MPs) <- paste('MP', 1:nMP)

MPs
Check(MPs)

## Fully Complete ----
Description(MPs) <- paste('Description', 1:nMP)
Color(MPs) <- c('red', 'blue')
Preset(MPs) <- list(All=c(1:4),
                   `First Two`=1:2)
MPs

Check(MPs)


## Multi-language ----
Label(MPs) <- list(en=paste('EN Label', 1:nMP),
                        es=paste('ES Label', 1:nMP),
                        fr=paste('FR Label', 1:nMP))

Description(MPs) <- list(en=paste('EN Description', 1:nMP),
                        es=paste('ES Description', 1:nMP),
                        fr=paste('FR Description', 1:nMP))


MPs

Check(MPs)

## Incomplete ----

### Code missing
MPs <- MPs()
Label(MPs) <- c('one', 'two')

MPs
Check(MPs)

### Label missing
MPs <- MPs()
Code(MPs) <- c('one', 'two')

MPs
Check(MPs)

### Code and Label missing

MPs <- MPs()
Description(MPs) <- c('one', 'two')
MPs
Check(MPs)


## Errors ----

## Code and Label different lengths
MPs <- MPs()
Code(MPs) <- paste('MP', 1:4)
Label(MPs) <- paste('MP', 1:2)


## Multi-language different lengths
MPs <- MPs()
Code(MPs) <- paste('MP', 1:4)
Label(MPs) <- list(en=paste('MP', 1:2))


## Multi-language invalid lang codes

MPs <- MPs()
Label(MPs) <- list(en=paste('EN MP', 1:2),
                   tt=paste('TT MP', 1:2))



# OMs ----

## Empty ----

object <- OMs()
OMs

Check(object)

## Minimum Complete ----
object <- OMs()

Factors(object) <- data.frame(Factor=c(rep('M',3),
                                    rep('h', 3),
                                    rep('Set',6)),
                           Level=c(0.1, 0.2, 0.3,
                                   0.69, 0.8, 0.88,
                                   'Reference',
                                   'R1',
                                   'R2',
                                   'R3a',
                                   'R3b',
                                   'R4'),
                           Description=c('Natural Mortality = 0.1',
                                         'Natural Mortality = 0.2',
                                         'Natural Mortality = 0.3',
                                         'Steepness = 0.69',
                                         'Steepness = 0.80',
                                         'Steepness = 0.88',
                                         'Reference OMs',
                                         'Robustness 1: Assumed 1 percent annual increase catchability (q), that is not accounted for in the standardization of the indices of abundance (historical and projection)',
                                         'Robustness 2: Same as R1, except only in the historical period',
                                         'Robustness 3a: Cyclical pattern in recruitment deviations in projection period; a proxy for impact of climate change on stock productivity',
                                         'Lower than expected recruitment deviations for first 15 years of projection period; a proxy for impact of climate change on stock productivity',
                                         'Illegal, unreported, or unregulated catches')
)


Design(object) <- data.frame(M=c(rep(c(0.1,0.2,0.3), each=3), rep(0.2, 5)),
                          h=c(rep(c(0.69,0.8,0.88),3), rep(0.8,5)),
                          Set=c(rep('Reference', 9),
                                'R1', 'R2', 'R3a', 'R3b', 'R4'))
object



Check(object)


## Fully Complete ----
Preset(object) <- list('Reference'=list(1:3, 1:3, 1),
                    'R0'=list(2, 2, 1),
                    'R1'=list(2, 2, 2),
                    'R2'=list(2, 2, 3),
                    'R3a'=list(2, 2, 4),
                    'R3b'=list(2, 2, 5),
                    'R4'=list(2, 2, 6))

object

 Check(object)


## Multi-language ----
object <- OMs()
Factors(object) <- list(en=data.frame(Factor=c(rep('M',3),
                                               rep('h', 3),
                                               rep('Set',6)),
                                      Level=c(0.1, 0.2, 0.3,
                                              0.69, 0.8, 0.88,
                                              'Reference',
                                              'R1',
                                              'R2',
                                              'R3a',
                                              'R3b',
                                              'R4'),
                                      Description=c('Natural Mortality = 0.1',
                                                    'Natural Mortality = 0.2',
                                                    'Natural Mortality = 0.3',
                                                    'Steepness = 0.69',
                                                    'Steepness = 0.80',
                                                    'Steepness = 0.88',
                                                    'Reference OMs',
                                                    'Robustness 1: Assumed 1 percent annual increase catchability (q), that is not accounted for in the standardization of the indices of abundance (historical and projection)',
                                                    'Robustness 2: Same as R1, except only in the historical period',
                                                    'Robustness 3a: Cyclical pattern in recruitment deviations in projection period; a proxy for impact of climate change on stock productivity',
                                                    'Lower than expected recruitment deviations for first 15 years of projection period; a proxy for impact of climate change on stock productivity',
                                                    'Illegal, unreported, or unregulated catches')),
                        es=data.frame(Factor=c(rep('M',3),
                                               rep('h', 3),
                                               rep('Set',6)),
                                      Level=c(0.1, 0.2, 0.3,
                                              0.69, 0.8, 0.88,
                                              'Reference',
                                              'R1',
                                              'R2',
                                              'R3a',
                                              'R3b',
                                              'R4'),
                                      Description=c('ES Natural Mortality = 0.1',
                                                    'ES Natural Mortality = 0.2',
                                                    'ES Natural Mortality = 0.3',
                                                    'ES Steepness = 0.69',
                                                    'ES Steepness = 0.80',
                                                    'ES Steepness = 0.88',
                                                    'ES Reference OMs',
                                                    'ES Robustness 1: Assumed 1 percent annual increase catchability (q), that is not accounted for in the standardization of the indices of abundance (historical and projection)',
                                                    'ES Robustness 2: Same as R1, except only in the historical period',
                                                    'ES Robustness 3a: Cyclical pattern in recruitment deviations in projection period; a proxy for impact of climate change on stock productivity',
                                                    'ES Lower than expected recruitment deviations for first 15 years of projection period; a proxy for impact of climate change on stock productivity',
                                                    'ES Illegal, unreported, or unregulated catches'))
)


object
Check(object)


## Incomplete ----

### No Factors
object <- OMs()
Design(object) <- data.frame(one=1, two=2)
object
Check(object)

### No Design
object <- OMs()
Factors(object) <- data.frame(one=1, two=2)
object
Check(object)

## Errors ----

# non matching Factors and Design
object <- OMs()

Factors(object) <- data.frame(Factor=c(rep('M',3),
                                       rep('h', 3),
                                       rep('Set',6)),
                              Level=c(0.1, 0.2, 0.3,
                                      0.69, 0.8, 0.88,
                                      'Reference',
                                      'R1',
                                      'R2',
                                      'R3a',
                                      'R3b',
                                      'R4'),
                              Description=c('Natural Mortality = 0.1',
                                            'Natural Mortality = 0.2',
                                            'Natural Mortality = 0.3',
                                            'Steepness = 0.69',
                                            'Steepness = 0.80',
                                            'Steepness = 0.88',
                                            'Reference OMs',
                                            'Robustness 1: Assumed 1 percent annual increase catchability (q), that is not accounted for in the standardization of the indices of abundance (historical and projection)',
                                            'Robustness 2: Same as R1, except only in the historical period',
                                            'Robustness 3a: Cyclical pattern in recruitment deviations in projection period; a proxy for impact of climate change on stock productivity',
                                            'Lower than expected recruitment deviations for first 15 years of projection period; a proxy for impact of climate change on stock productivity',
                                            'Illegal, unreported, or unregulated catches')
)

Design(object) <- data.frame(M=1, h=2, Sett=3)

## UP TO HERE
Design(object) <- data.frame(M=1, h=2, Set=3)

object
Check(object)









# Boxplot ----

# Kobe ----

# Quilt ----

# Spider ----

# Timeseries ----

# Tradeoff ----

# Slick ----


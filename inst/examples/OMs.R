
myOMs <- OMs()

Metadata(myOMs) <- data.frame()


# Design <- expand.grid(M=c(0.1, 0.2, 0.3), h=c(0.7,0.9))
#' Description <- list(c('Natural mortality (M) = 0.1',
#'                       'Natural mortality (M) = 0.2',
#'                       'Natural mortality (M) = 0.3'),
#'                     c('Steepness (h) = 0.7',
#'                       'Steepness (h) = 0.9'))
#' Label <- list(c(M=0.1, M=0.2, M=0.3),
#'                c(h=0.7, h=0.9))
#' myOMs <- OMs(Design, Description, Label)

#' mySlick <- Slick()
#' Design(mySlick) <- data.frame(M=c(0.1, 0.2, 0.3),
#'                               h=c(0.6, 0.7, 0.8)
#'                              )
#' Design(mySlick)

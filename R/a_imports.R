#' @importFrom graphics lines par points polygon text
#' @importFrom methods new slot slot<- slotNames
#' @importFrom stats median quantile time
#' @importFrom utils packageVersion
#' @importFrom grDevices colors rgb
NULL


if(getRversion() >= "2.15.1")
  utils::globalVariables(c(
    'Factor',
    'Heading',
    'Level',
    'Lower1',
    'Lower2',
    'MP',
    'MPtxt',
    'Median',
    'Upper1',
    'Upper2',
    'fill.col',
    'lab',
    'line.col',
    'low1' ,
    'low2',
    'lwd',
    'm',
    'quadrant',
    'upp1',
    'upp2',
    'x',
    'y',
    'value'
    )
    )

boxplot <- Boxplot(Code=c('PI1', 'PI2', 'PI3'),
                   Label=c('Performance Indicator 1',
                         'Performance Indicator 2',
                         'Performance Indicator 3'),
                   Description = c('This is the description for PI 1',
                                   'This is the description for PI 2',
                                   'This is the description for PI 3'),
                   Value=array(runif(3000), dim=c(6, 5, 4, 3)),
                   Preset=list('PI and P2'=1:2)

)

boxplot

Code(boxplot) <- c('LTY', 'STY', 'P50')
Metadata(boxplot)


# Add to `Slick` object
slick <- Slick()
Boxplot(slick) <- boxplot

plotBoxplot(slick)

plotBoxplot(slick, type='violin')

plotBoxplot(slick, type='both')

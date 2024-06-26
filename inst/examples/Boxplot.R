boxplot <- Boxplot(Code=c('PI1', 'PI2', 'PI3'),
                   Label=c('Performance Indicator 1',
                         'Performance Indicator 2',
                         'Performance Indicator 3'),
                   Description = c('This is the description for PI 1',
                                   'This is the description for PI 2',
                                   'This is the description for PI 3'),
                   Value=array(runif(3000), dim=c(10, 10, 10,3)),
                   Preset=list('PI and P2'=1:2)

)

boxplot

Code(boxplot)
Metadata(boxplot)

# Value(boxplot)
Value(boxplot) <- array()

Preset(boxplot)
Preset(boxplot) <- list()

boxplot


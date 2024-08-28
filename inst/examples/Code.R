boxplot <- Boxplot()

Code(boxplot) <- c('PI1', 'PI2', 'PI3')
Label(boxplot) <- c('Performance Indicator 1',
                    'Performance Indicator 2',
                    'Performance Indicator 3')
Description(boxplot) <- c('This is the description for PI 1',
                          'This is the description for PI 2',
                          'This is the description for PI 3')


Code(boxplot)
Label(boxplot)
Description(boxplot)

# Multi-Language
Description(boxplot) <- list(en=c('This is the English description for PI 1',
                                  'This is the English description for PI 2',
                                  'This is the English description for PI 3'),
                             es=c('This is the Spanish description for PI 1',
                                  'This is the Spanish description for PI 2',
                                  'This is the Spanish description for PI 3'),
                             fr=c('This is the French description for PI 1',
                                  'This is the French description for PI 2',
                                  'This is the French description for PI 3')
)

Description(boxplot)
Description(boxplot, 'es')
Description(boxplot, 'fr')

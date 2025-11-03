
myMPs <- MPs()
Code(myMPs) <- c('MP1', 'MP2', 'MP3')
Label(myMPs) <- c('Management Procedure 1',
                  'Management Procedure 2',
                  'Management Procedure 3')
Description(myMPs) <- c('This is the description for Management Procedure 1',
                        'This is the description for Management Procedure 2',
                        'This is the description for Management Procedure 3')

Preset(myMPs) <- list(All=1:3, FirstTwo=1:2)


myMPs

# Multi-language
Description(myMPs) <- list(en=c('This is the English description for Management Procedure 1',
                             'This is the English description for Management Procedure 2',
                             'This is the English description for Management Procedure 3'),
                           es=c("This is the Spanish description for Management Procedure 1",
                             "This is the Spanish description for Management Procedure 2",
                             "This is the Spanish description for Management Procedure 3"),
                           fr=c("This is the French description for Management Procedure 1",
                             "This is the French description for Management Procedure 2",
                             "This is the French description for Management Procedure 3"),
                           pt=c("This is the Portuguese description for Management Procedure 1",
                                "This is the Portuguese description for Management Procedure 2",
                                "This is the Portuguese description for Management Procedure 3")
                           )

Metadata(myMPs)
Metadata(myMPs, 'es')
Metadata(myMPs, 'fr')
Metadata(myMPs, 'pt')

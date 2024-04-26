
myMPs <- MPs()
Metadata(myMPs) <- data.frame(Code=c('MP1', 'MP2'),
                              Label=c('Management Procedure 1', 'Management Procedure 2'),
                              Description=c('This is the description for Management Procedure 1',
                                            'This is the description for Management Procedure 2')
                              )
Preset(myMPs) <- list(All=1:3, FirstTwo=1:2)


# Multi-language
df_es <- Metadata(myMPs)
df_fr <- Metadata(myMPs)
df_es$Description <- c("This is the Spanish description for Management Procedure 1",
                       "This is the Spanish description for Management Procedure 2")
df_fr$Description <- c("This is the French description for Management Procedure 1",
                       "This is the French description for Management Procedure 2")

Metadata(myMPs) <- list(en=Metadata(myMPs),
                        es=df_es,
                        fr=df_fr)


Metadata(myMPs)
Metadata(myMPs, 'es')
Metadata(myMPs, 'fr')

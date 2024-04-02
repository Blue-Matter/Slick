
# devtools::load_all()

library(Slick)

mySlick <- Slick(Title='This is the title',
                 Subtitle='This is the subtitle')

Title(mySlick)

Title(mySlick) <- list(en='This is the title in English',
                       es='This is the title in Spanish',
                       fr='This is the title in French')

Title(mySlick)
Title(mySlick, 'en')
Title(mySlick, 'fr')

Subtitle(mySlick) <- list(en='This is the subtitle in English',
                          es='This is the subtitle in Spanish',
                          fr='This is the subtitle in French')

Author(mySlick) <- c('Author 1', 'Author 2', 'Author 3')
Email(mySlick) <- c('author1@email.com', 'author2@email.com', '')
Institution(mySlick) <- c('Institution 1', 'Institution 1', 'Institution 2')

Introduction(mySlick) <- '
This is the first paragraph of the Introduction. It can include Markdown such as **bold text**, *italic text*, [links](www.link.com).

This is the second paragraph.

And this is the third.
'

Introduction(mySlick) <- list(en='
                              Multiple languages use a list. This is the English Introduction.

                              This is the second paragraph.
                              ',
                              es='This is the first paragraph of the Spanish Introduction.

                              This is the second paragraph of the Spanish Introduction',
                              fr='This is the first paragraph of the French Introduction.

This is the second paragraph of the French Introduction
                              ')


MPs(mySlick) <- data.frame(Code=c('CC', 'SP', 'SCA'),
                           Label=c('Catch Curve', 'Surplus Production', 'Statistical Catch-at-Age'),
                           Description=c('Estimates current fishing mortality using a catch curve, and adjusts the effort recommendation to reach a target F level.',
                                         'Surplus Production estimation model linked with a harvest control rule that sets a TAC',
                                         'Statistical Catch-at-Age estimation model linked with a harvest control rule that sets a TAC'))


# extra options
MPs(mySlick) <- data.frame(Code=c('CC', 'SP', 'SCA'),
                           Label=c('Catch Curve', 'Surplus Production', 'Statistical Catch-at-Age'),
                           Description=c('Estimates current fishing mortality using a catch curve, and adjusts the effort recommendation to reach a target F level.',
                                         'Surplus Production estimation model linked with a harvest control rule that sets a TAC',
                                         'Statistical Catch-at-Age estimation model linked with a harvest control rule that sets a TAC'),
                           Color=colors()[runif(3, 1, 657)],
                           Default=c(TRUE, TRUE, FALSE))

# multilanguages
MPs(mySlick) <- list(en=data.frame(Code=c('CC', 'SP', 'SCA'),
                                   Label=c('Catch Curve', 'Surplus Production', 'Statistical Catch-at-Age'),
                                   Description=c('Estimates current fishing mortality using a catch curve, and adjusts the effort recommendation to reach a target F level.',
                                                 'Surplus Production estimation model linked with a harvest control rule that sets a TAC',
                                                 'Statistical Catch-at-Age estimation model linked with a harvest control rule that sets a TAC'),
                                   Color=colors()[runif(3, 1, 657)],
                                   Default=c(TRUE, TRUE, FALSE)),

                     es=data.frame(Code=c('CC', 'SP', 'SCA'),
                                   Label=c('Only Label', 'and Description', 'should be in a different language.'),
                                   Description='This is the Spanish Description',
                                   Color=colors()[runif(3, 1, 657)],
                                   Default=c(TRUE, TRUE, FALSE))
)


saveRDS(mySlick, 'C:/users/adrian/downloads/slick.slick')


MPs(mySlick, 'en')
MPs(mySlick, 'es')


# TODO add validation checks

# OMs

## Metadata
metadata <- data.frame(Factor=c(rep('Example.1', 2),
                          rep('Example.2', 3)),
                 Level=c(0.1, 0.2, 10, 20, 30),
                 Description=c('Description of Example.1 Level 1',
                               'Description of Example.1 Level 2',
                               'Description of Example.2 Level 1',
                               'Description of Example.2 Level 2',
                               'Description of Example.2 Level 2')
)


## Design
design <- data.frame(Example1=c(0.1,0.2),
                     Example.2=c(rep(10,2), rep(20,2), rep(30,2)),
                     Default=c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
)

OMs(mySlick) <- OMs(Metadata=metadata,
                    Design=design)

# Quilt

metadata <- data.frame(Code,
                       Label,
                       Description)

metadata <- data.frame(Code,
                       Label,
                       Description,
                       Default,
                       MinValue,
                       MaxValue)



Quilt(mySlick) <- Quilt(Metadata=df,
                        Value=array(),
                        MinColor='white',
                        MaxColor='darkblue'
                        )

Metadata(Quilt(mySlick), 'es')

Spider

Boxplot

Kobe

Timeseries



slotNames('Slick')
mySlick

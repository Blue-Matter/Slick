
HomeServer <- function(id, i18n) {
  moduleServer(id,
               function(input, output, session) {


                 output$welcome <- renderUI({
                   tagList(
                     box(title=h2(i18n()$t('Welcome')),
                         width=6,
                         solidHeader=TRUE,
                         status = "primary",
                         h3(i18n()$t('Welcome to Slick')),
                         p(i18n()$t('Slick is a decision analysis tool that presents the outcomes of candidate harvest strategies across various states of nature, accounting for uncertainty. Slick is interactive and allows users to filter results live in order to explore robustness and performance.')),
                         p(i18n()$t('While Slick can be applied to any decision analysis context it was specifically designed to investigate the performance of harvest strategies tested by management strategy evaluation (MSE).')),
                         h3(i18n()$t('How to use Slick')),
                         p(i18n()$t('Instructions here ...'))
                     )
                   )

                 })




               }
  )
}



HomeUI <- function(id, label="home") {

  ns <- NS(id)


  tagList(
    usei18n(i18n),
    fluidRow(

      htmlOutput(ns('welcome'))
    )
  )
}



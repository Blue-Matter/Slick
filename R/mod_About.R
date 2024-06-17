#' About UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_About_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('about'))
  )
}

#' About Server Functions
#'
#' @noRd
mod_About_server <- function(id, i18n){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$about <- renderUI({
      i18n <- i18n()
      tagList(
        h4(i18n$t('About Slick')),
        p(i18n$t('Slick was developed by'), a(href='https://www.bluematterscience.com/', 'Blue Matter Science',  target="_blank"),
          i18n$t("and designed and commissioned by"), a(href='https://oceanfdn.org/', "The Ocean Foundation's",  target="_blank"),
          i18n$t('International Fisheries Conservation Project and'), a(href='https://www.harveststrategies.org', "harveststrategies.org,",  target="_blank"),
          i18n$t('with support from'), a(href='https://www.pewtrusts.org/', 'The Pew Charitable Trusts,',  target="_blank"),
          i18n$t('and the'),  a(href='https://www.fao.org/in-action/commonoceans/what-we-do/tuna/en/', 'Common Oceans Tuna Fisheries Project,',  target="_blank"),
          i18n$t('which is funded by'),  a(href='https://www.thegef.org/what-we-do/topics/areas-beyond-national-jurisdiction', 'GEF',  target="_blank"),
          i18n$t('and implemented by the'),  a(href='https://www.fao.org/in-action/commonoceans/en/', 'FAO.',  target="_blank")),
        p(i18n$t('The prototype figure designs were developed by'), a(href="https://www.5wgraphics.com/",  '5W Infographics.',  target="_blank")),
        p(i18n$t('Slick is under going further development. All feedback is welcome. Please contact'),
          a(href="mailto:smiller@oceanfdn.org?&subject=Slick Development", 'Shana Miller'),
          i18n$t('with any comments or suggestions for further development.')),
        br(),
        fluidRow(align = "center",
                 column(4,
                        tags$a(href='https://harveststrategies.org/', target="_blank",
                               tags$div(
                                 tags$img(src='www/img/logos/HSlogo.png', height = '100', width ='100')
                               )
                        )
                 ),
                 column(1),
                 column(4,
                        tags$a(href='https://www.bluematterscience.com/', target="_blank",
                               tags$div(
                                 tags$img(src='www/img/logos/Blue_Matter_colour.png', height = '114', width ='216')

                               )
                        )
                 )
        ),
        br()
      )
    })

  })
}

## To be copied in the UI
# mod_About_ui("About_1")

## To be copied in the server
# mod_About_server("About_1")

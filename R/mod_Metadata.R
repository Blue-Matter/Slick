

make_author_email <- function(Author, Email) {
  ind <- which(nchar(Email)>0)
  if (length(ind)>0) {
    Author[ind] <- paste0('<a href = "mailto: ', Email[ind], '">', Author[ind],'</a>')
  }
  Author
}


#' Metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Metadata_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('main'))
  )
}

#' Metadata Server Functions
#'
#' @noRd
mod_Metadata_server <- function(id, i18n, Slick_Object){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_MP_Info_server("MPmetadata", i18n, Slick_Object)
    mod_OM_Info_server("OMmetadata", i18n, Slick_Object)
    mod_PM_Info_server("PMmetadata", i18n, Slick_Object)

    output$main <- renderUI({
      i18n <- i18n()
      tagList(
        shinydashboard::box(title=h3(strong(i18n$t('Overview'))),
                                width=12,
                                solidHeader=TRUE,
                                status = "primary",
                                uiOutput(ns('tabsetpanel'))
        )
        # mod_links_ui(ns(id))
      )
    })

    # main tabBox panels ----
    output$tabsetpanel <- renderUI({
      i18n <- i18n()
      tagList(
        fluidRow(
          shinydashboard::tabBox(width=6,
                                 tabPanel(title=h5(strong(i18n$t('Metadata'))),
                                          uiOutput(ns('metadata'))
                                 ),
                                 tabPanel(title=h5(strong(i18n$t('Management Procedures'))),
                                          mod_MP_Info_ui(ns("MPmetadata"))
                                 ),
                                 tabPanel(title=h5(strong(i18n$t('Operating Models'))),
                                          mod_OM_Info_ui(ns("OMmetadata"))
                                 ),
                                 tabPanel(title=h5(strong(i18n$t('Performance Indicators'))),
                                          mod_PM_Info_ui(ns("PMmetadata"))
                                 )
          ),
          uiOutput(ns('plotinfo'))
        )
      )
    })

    # ---- metadata tab ----
    output$author_info <- renderText({
      slick <- Slick_Object()
      i18n <- i18n()

      df <- data.frame(Author=make_author_email(Author=Author(slick),
                                                Email=Email(slick)),
                       Institution=Institution(slick))

      if (nchar(df$Institution) |> sum() ==0)
        df$Institution <- NULL

      knitr::kable(df,
                   format='html',
                   escape = FALSE) |>
        kableExtra::row_spec(0,bold=TRUE) |>
        kableExtra::kable_styling()

    })

    output$metadata <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      tagList(
        column(12,
               br(),
               h3(Title(slick, i18n$get_translation_language())),
               h4(Subtitle(slick, i18n$get_translation_language())),
               p(strong(i18n$t('Created:')), Date(slick)),
               fluidRow(
                 column(12, tableOutput(ns("author_info")))
               ),
               lapply(lapply(Introduction(slick), HTML), tags$p)
        )
      )
    })

    output$plotinfo <- renderUI({
      i18n <- i18n()
      slick <- Slick_Object()
      tagList(
        shinydashboard::box(width=6,
                            status = "primary",
                            title=strong(i18n$t('About the Plots')),
                            p('This Slick file includes the following plots:'),
                            br(),
                            p('TODO: Add descriptions and links for the plots that are included in this slick file')
                            )
      )
    })



  })
}

## To be copied in the UI
# mod_Metadata_ui("Metadata_1")

## To be copied in the server
# mod_Metadata_server("Metadata_1")

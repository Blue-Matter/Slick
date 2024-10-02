
check_required_packages <- function() {
  app_packages <- c('fresh',
                    'colourpicker',
                    'colorspace',
                    'cowplot',
                    'esquisse',
                    'flextable',
                    'fresh',
                    'golem',
                    'kableExtra',
                    'knitr',
                    'shiny',
                    'shiny.i18n',
                    'shinyalert',
                    'shinyBS',
                    'shinycssloaders',
                    'shinydashboard',
                    'shinydashboardPlus',
                    'shinyhelper',
                    'shinyjs',
                    'shinyWidgets',
                    'rmarkdown',
                    'waiter')

  checks <- lapply(app_packages, requireNamespace, quietly = TRUE) |> unlist()
  df <- data.frame(Package=app_packages, Installed=checks) |>
    dplyr::filter(Installed==FALSE)
  if (nrow(df)>1) {

    cli::cli_alert_danger('This function requires the following package(s) to be installed:')
    cli::cli_ol()
    cli::cli_li(as.list(df$Package))
    cli::cat_line()
    cli::cli_inform('Install the missing packages?')
    chk <- utils::menu(c('Yes', 'No'), FALSE)
    if (chk==1) {
      install.packages(df$Package)
    } else {
      stop('Missing packages', call.=FALSE)
    }

  }

}




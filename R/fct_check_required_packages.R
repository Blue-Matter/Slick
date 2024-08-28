
check_required_packages <- function() {
  app_packages <- c('fresh',
                    'colourpicker',
                    'colorspace',
                    'esquisse',
                    'flextable',
                    'fresh',
                    'golem',
                    'knitr',
                    'MSEtool',
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
  if (nrow(df)>1)
    stop('This function requires the following package(s) to be installed: \n', paste(df$Package, collapse = '\n'))

}

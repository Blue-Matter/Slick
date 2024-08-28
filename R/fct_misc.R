
loading_spinner <- function(ui_element, ...) {
  if (requireNamespace("ggplot2", quietly = TRUE))
    return(shinycssloaders::withSpinner(ui_element, ...))
  ui_element
}


slickVersion <- function() {
  if (requireNamespace('utils', quietly = TRUE))
    return(utils::packageVersion('Slick'))
  NULL
}

icon <- function(name, class=NULL, lib = "font-awesome", ...) {
  suppressMessages(shiny::icon(name, class, lib, ...))
}

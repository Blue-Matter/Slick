#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(port=3838, host='0.0.0.0'),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}


#' Run a Shiny Application
#'
#' \code{Slick} runs the Slick Shiny Application
#'
#' @export
Slick <- function(...) {
  run_app(...)
  # temp <- try(class(app), silent=TRUE)
  # if (class(temp) == "try-error") app <- deparse(substitute(app))
  # Apps <- list.files(system.file("shiny_apps", package = "Slick"))
  # validAppMsg <- paste0("Valid examples are:\n '", paste(Apps, collapse = "', '"), "'")
  # appDir <- system.file("shiny_apps", app, package = "Slick")
  # shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE, ...)
}



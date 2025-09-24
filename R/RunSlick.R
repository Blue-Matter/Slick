#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @noRd
run_app <- function(
    onStart = NULL,
    options = list(port=3838, host='0.0.0.0',
                   launch.browser = TRUE),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  golem::with_golem_options(
    app = shiny::shinyApp(
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


#' Run the Slick App in the local browser
#'
#' An online hosted version of the Slick App is available
#' [here](https://shiny.bluematterscience.com/app/slick)
#'
#' `App()` runs the Slick Shiny Application
#' @param ... Additional arguments. To load a object directly in the App,
#' use `App(slick=myslick)` where `myslick` is your [Slick-class()] object.
#' @return None
#' @export
App <- function(...) {
  check_required_packages()
  run_app(...)
}



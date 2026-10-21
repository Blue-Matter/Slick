#' util
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
helper2 <- function(shiny_tag, icon = "question-circle", colour = NULL,
                    type = "markdown", title = "", content = "", size = "m",
                    buttonLabel = "Okay", easyClose = TRUE, fade = FALSE, ...) {

  if (!(type %in% c("inline", "markdown"))) {
    stop("type must be 'inline' or 'markdown'")
  }
  if (!(size %in% c("s", "m", "l"))) {
    stop("size must be in c('s', 'm', 'l')")
  }
  if (type == "inline") {
    content <- paste(content, sep = "", collapse = "<br>")
  }
  else {
    content <- paste(content, ".md", sep = "")
  }
  if (!is.null(colour)) {
    colour <- paste0("color: ", colour, ";")
  }
  help_icon <- shiny::icon(name = icon, class = "shinyhelper-icon")
  help_icon$attribs$class <- "far fa-question-circle shinyhelper-icon"
  help_icon <- shiny::tagAppendAttributes(tag = help_icon,
                                          `data-modal-size` = size, `data-modal-type` = type, `data-modal-title` = title,
                                          `data-modal-content` = content, `data-modal-label` = buttonLabel,
                                          `data-modal-easyclose` = easyClose, `data-modal-fade` = fade)
  help_icon <- shiny::div(help_icon, class = "shinyhelper-container",
                          style = colour, ...)
  shiny::addResourcePath(prefix = "shinyhelper", directoryPath = system.file("assets",
                                                                             package = "shinyhelper"))
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet",
                                                                    href = "shinyhelper/shinyhelper.css"), shiny::tags$script(src = "shinyhelper/shinyhelper.js"))),
                 shiny::div(shiny_tag, help_icon, class = "shinyhelper-wrapper"))

}


#' Alternative text for plots
#'
#' Builds a short text description of a plot for screen readers. Used as the
#' `alt` attribute of the `<img>` tag created by `renderPlot`.
#'
#' @param slick A `Slick` object (or NULL)
#' @param plot Name of the plot type: 'Boxplot', 'Kobe', 'Spider', 'Timeseries' or 'Tradeoff'
#' @param PI Optional integer index of the Performance Indicator(s) shown in the plot
#' @param detail Optional character string with extra details about the plot
#'
#' @return A character string
#' @noRd
plot_alt_text <- function(slick, plot, PI=NULL, detail=NULL) {
  tryCatch({
    if (is.null(slick))
      return(paste(plot, 'plot'))

    obj_codes <- function(codes) {
      if (is.list(codes)) codes <- codes[[1]]
      as.character(codes)
    }

    MP_codes <- obj_codes(slick@MPs@Code)
    nOM <- nrow(slick@OMs@Design)
    PI_codes <- obj_codes(methods::slot(slick, plot)@Code)
    if (!is.null(PI))
      PI_codes <- PI_codes[PI]

    plot_name <- switch(plot,
                        Boxplot='Boxplot',
                        Kobe='Kobe plot',
                        Spider='Spider (radar) plot',
                        Timeseries='Time series plot',
                        Tradeoff='Trade-off plot',
                        plot)

    if (nOM < 1 || all(MP_codes == '') || all(PI_codes == ''))
      return(plot_name)

    paste0(plot_name,
           if (!is.null(detail)) paste0(' ', detail),
           ' showing ', paste(PI_codes, collapse=', '),
           ' for ', length(MP_codes),
           ifelse(length(MP_codes)==1, ' management procedure (', ' management procedures ('),
           paste(MP_codes, collapse=', '), ') across ', nOM,
           ifelse(nOM==1, ' operating model.', ' operating models.'))
  }, error=function(e) paste(plot, 'plot'))
}

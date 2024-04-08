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

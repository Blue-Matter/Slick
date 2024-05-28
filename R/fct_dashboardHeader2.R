tagAssert <- shinydashboardPlus:::tagAssert

# modified shinydashboardPlus::dashboardHeader to include the words 'Filter'
dashboardHeader2 <- function (..., title = NULL, titleWidth = NULL, disable = FALSE,
                              .list = NULL, leftUi = NULL, controlbarIcon = shiny::icon("gears"),
                              fixed = FALSE) {
  items <- c(list(...), .list)
  lapply(items, tagAssert, type = "li", class = "dropdown")
  if (!is.null(leftUi)) {
    left_menu_items <- lapply(seq_along(leftUi), FUN = function(i) {
      left_menu_item <- leftUi[[i]]
      name <- left_menu_item$name
      class <- left_menu_item$attribs$class
      if (name != "li" || !is.null(class) || class != "dropdown") {
        dropdownTag <- shiny::tags$li(class = "dropdown")
        left_menu_item <- shiny::tagAppendChild(dropdownTag,
                                                left_menu_item)
        left_menu_item <- shiny::tagAppendAttributes(left_menu_item,
                                                     style = "margin-top: 7.5px; margin-left: 5px; margin-right: 5px;")
      }
      else {
        left_menu_item
      }
    })
  }
  else {
    left_menu_items <- leftUi
  }
  titleWidth <- shiny::validateCssUnit(titleWidth)
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    custom_css <- shiny::tags$head(shiny::tags$style(shiny::HTML(gsub("_WIDTH_",
                                                                      titleWidth, fixed = TRUE, "@media (min-width: 768px) {\n              .main-header > .navbar {\n                margin-left: _WIDTH_;\n              }\n              .main-header .logo {\n                width: _WIDTH_;\n              }\n             }\n              "))))
  }
  shiny::tags$header(class = "main-header", custom_css, style = if (disable)
    "display: none;", shiny::tags$span(class = if (is.null(title))
      "logo hidden-xs"
      else "logo", title), shiny::tags$nav(class = paste0("navbar navbar-",
                                                          if (fixed)
                                                            "fixed"
                                                          else "static", "-top"),
                                           role = "navigation",
                                           shiny::tags$span(shiny::icon("bars"), style = "display:none;"),
                                           shiny::tags$a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas", role = "button", shiny::tags$span(class = "sr-only", "Toggle navigation")),
                                           shiny::tags$div(class = "navbar-custom-menu", style = "float: left; margin-left: 10px;", shiny::tags$ul(class = "nav navbar-nav",left_menu_items)),
                                           shiny::tags$div(class = "navbar-custom-menu",
                                                           shiny::tags$ul(class = "nav navbar-nav",
                                                                          items,
                                                                          shiny::tags$li(
                                                                            shiny::tags$a(href = "#",`data-toggle` = "control-sidebar", shiny::tags$span('Filter'), controlbarIcon))))))
}

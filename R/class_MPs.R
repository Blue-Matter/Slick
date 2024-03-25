# ---- Class MPs ----
#' Create a new `MPs` object
#'
#' @return An object of class `MPs`
#' @export
#'
MPs <- setClass("MPs",
                slots=c(Label='character_list',
                        Description='character_list',
                        Link='character',
                        Color='character',
                        Default='numeric'
                )
)


setMethod("initialize", "MPs", function(.Object,
                                        Label=NULL,
                                        Description=NULL,
                                        Link=NULL,
                                        Color=NULL,
                                        Default=NULL) {
  if (!is.null(Label)) {
    .Object@Label <- Label
  }

  if (!is.null(Description)) {
    .Object@Description <- Description
  }

  if (!is.null(Link)) {
    .Object@Link <- Link
  }

  if (!is.null(Color)) {
    .Object@Color <- Color
  }

  if (!is.null(Default)) {
    .Object@Default <- Default
  }

  .Object
})


validMPs <- function(object) {
  TRUE
}

setValidity('MPs', validMPs)




newMPs <- function(Label=NULL,
                   Description=NULL,
                   Link=NULL,
                   Color=NULL,
                   Default=NULL) {
  new('MPs', Label, Description, Link, Color, Default)
}



#' An S4 class to represent a bank account
#'
#' @slot balance A length-one numeric vector
#' @details
#' Additional details...
#'
#' @export
setClass("Account",
                    slots = list(balance = "numeric")
)

#' @describeIn Account description
#' @export
setGeneric("Account", function(x) standardGeneric("Account"))

#' @describeIn Account description
#' @export
setGeneric("Account<-", function(x, value) standardGeneric("Account<-"))

#' @describeIn Account description
#' @export
setMethod("Account<-", "numeric", function(x, value) {
  x@balance <- value
  x
})


#' @describeIn Account description
#' @export
setMethod("Account", "array", function(x) {
  x
})

#' Generic function for `MPs` objects
#'
MPs <- setClass("MPs",
                slots=c(Metadata='dataframe_list',
                        Preset='list'
                )
)

validMPs <- function(object) {

}

setValidity('MPs', validMPs)

# initialize ----
setMethod("initialize", "MPs", function(.Object,
                                        Metadata=NULL,
                                        Preset=NULL) {

  if (!is.null(Metadata)) {
    .Object@Metadata <- Metadata
  }

  if (!is.null(Preset)) {
    .Object@Preset <- Preset
  }

  .Object
})

newMPs <- function(Metadata=NULL,
                   Preset=NULL) {
  new('MPs', Metadata, Preset)
}


#' MPs S4 class and functions
#'
#' An `MPs` object contains information about the management procedures (MPs)
#' in the [Slick()] object.
#'
#' @slot Metadata A data.frame with a specific structure, describing the details of the management procedures. See `Details` section.
#' @slot Preset An optional named list of preset buttons for filters in the [App()]. See `Details` section.
#'
#' @details
#'
#' ### Metadata
#' The `Metadata` data.frame must have n MPs rows and the following columns:
#'    - Code: A short character string for identifying the MPs. Used in the [App()] in places
#'    where there is no room for a full label.
#'    - Label: A long character string (one word or max two words) for identifying the MPs.
#'    - Description: A longer character string providing a description of the MPs.
#'    - Color: A character vector with the colors to use to display the MPs in the [App()]. Optional.
#'    If not provided, the App will try to set some sensible colors.
#'
#' For multi-language support, the `Metadata` slot can be populated with a named
#' list: names `en`, `es`, and `fr` for English, Spanish, and French respectively.
#' The data.frame within each list element must have identical structure; i.e., the same number of rows
#' and the column names in each list element must be identical.
#'
#' The Metadata slot is accessed with `Metadata(MPs)` and modified with `Metadata(MPs) <- data.frame()`
#'
#' ### Preset
#' The `Preset` slot is an optional named list to add preset buttons for the Management
#' Procedure filters in the [App()]. The name of the list element will appear as a button
#' in the [App()]. Each list element should contain numeric values specifying the MPs to include.
#' The values must be <= n MPs.
#'
#' The Preset slot is accessed with `Preset(MPs)` and modified with `Preset(MPs) <- `list()`
#'
#' @example inst/examples/MPs.R
setClass("MPs",
                slots=c(Metadata='dataframe_list',
                        Preset='list'
                )
)

check_mps_dataframe <- function(df) {
  if (nrow(df)>0) {
    nms <- names(df)
    if (!all(c('Code', 'Label', 'Description') %in% nms)) {
      return('Metadata must be a data.frame with columns: Code, Label, and Description')
    }
  }

}

validMPs <- function(object) {
  df <- object@Metadata

  if (inherits(df, 'data.frame')) {
    nMPs <- nrow(df)
    check_mps_dataframe(df)
  } else if (inherits(df, 'list')) {
    chk <- lapply(df, check_mps_dataframe) |> unlist()
    if (!is.null(chk))
      return(chk)
    nMPs <- lapply(df, nrow) |> unlist()
    if (!all(nMPs==nMPs[1]))
      return('Different number of MPs in the named list')
  }

  # TODO - check preset
  nmps <- max(nMPs)
  pr <- Preset(object)
  if (length(pr)>0) {
    pr_max <- lapply(pr, max) |> unlist() |> max()
    if (pr_max>nmps)
      return('Preset is incorrect. Cannot have values large than nMPs')
  }

  TRUE
}

setValidity('MPs', validMPs)

showMPs <- function(MPs) {
  cat('An object of class `MPs` \n\n')
  mps <- Metadata(MPs)
  cat('Metadata:\n')
  print(mps)
  pr <- Preset(MPs)
  cat('\nPreset:')
  if (length(pr)<1) {
    cat(' None')
  } else {
    cat('\n')
    print(Preset(MPs))
  }

}

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




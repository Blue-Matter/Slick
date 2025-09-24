
#' Get Case Studies from the SlickLibrary GitHub Repo
#'
#' @return A data.frame for `get_casestudies` and a `Slick` object for `download_casestudy`
#' @export
get_casestudies <- function() {
  if (!requireNamespace('httr', quietly = TRUE))
    stop('Package `httr` required for this function')

  req <- httr::GET("https://api.github.com/repos/blue-matter/slicklibrary/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = FALSE)

  type <- unlist(lapply(httr::content(req)$tree, "[", "type"), use.names = FALSE)
  filelist <- filelist[type=='blob']

  size <- unlist(lapply(httr::content(req)$tree, "[", "size"), use.names = FALSE)

  ind <- grepl('Slick_Objects/', filelist)
  slick_files <- filelist[ind]
  size <- size[ind]
  names <- lapply(strsplit(slick_files, 'Slick_Objects/'), '[[',2) |> unlist()
  names <- gsub('.rda', '', names)
  names <- gsub('.slick', '', names)
  names <- gsub('_', ' ', names)

  data.frame(Name=names, File=slick_files, Size=round(size/1e6,2))
}

#' @describeIn get_casestudies download a case study file
#' @param name The name of the case study to download. `Name` column from `get_casestudies()`
#' @param case_studies optional. Dataframe returned by `get_casestudies()`
#' @param dir Optional. Directory to save the file. Defaults to a temporary directory
#' @param silent Logical. Print out messages?
#' @param object Logical. Return the Slick object? Default downloads Slick object to a temporary location,
#' loads and returns the Slick object, and then deletes downloaded file.
#' @param delete Logical. Delete the downloaded file after function finishes? Only useful if `object = TRUE`
#' @return The downloaded `Slick` object if `object==TRUE`, otherwise nothing.
#' @examplesIf interactive()
#' case_studies <- get_casestudies()
#' slick <- download_casestudy(case_studies$Name[1])
#'
#' @export
download_casestudy <- function(name, case_studies=NULL, dir=NULL,
                               silent=FALSE,
                               object=TRUE, delete=object) {
  if (is.null(case_studies))
    case_studies <- get_casestudies()
  ind <- match(name, case_studies$Name)
  if (is.na(ind) | length(ind)<1) {
    cli::cli_abort(paste(name, ' not found in Slick Library. \n\nOptions are: ',
                   paste(case_studies$Name, collapse=', ')))
  }

  file <- case_studies$File[ind]
  url <- paste0('https://raw.githubusercontent.com/blue-matter/slicklibrary/master/', file)
  url <- gsub(' ', '%20', url)
  if (is.null(dir)) {
    dir <- tempdir()
  }

  out_file <- file.path(dir, basename(file))
  if (!silent) cli::cli_alert_info(paste('Downloading', name, 'to', out_file))

  utils::download.file(url, out_file, mode ="wb", quiet=silent)

  if (delete)
    on.exit(file.remove(out_file))
  if (object)
    return(readRDS(out_file))

}


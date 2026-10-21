
#' List and Download the Example Case Studies
#'
#' The example case studies shown in the [App()] are [Slick-class()] objects stored
#' in the SlickLibrary GitHub repository: <https://github.com/Blue-Matter/SlickLibrary>.
#' `get_casestudies()` lists the available case studies, and
#' `download_casestudy()` downloads a case study and loads it into the R session.
#'
#' Both functions require an internet connection and the `httr` package.
#'
#' @return
#' `get_casestudies()` returns a `data.frame` with one row per case study and columns:
#' \describe{
#'   \item{Name}{The name of the case study. Use this for the `name` argument of `download_casestudy()`}
#'   \item{File}{The path of the file in the SlickLibrary repository}
#'   \item{Size}{The size of the file in MB}
#' }
#'
#' `download_casestudy()` returns the [Slick-class()] object if `object=TRUE`,
#' otherwise `NULL` (invisibly), and the file is saved in `dir`.
#'
#' @seealso [WSKJ] for an example `Slick` object included with the package
#' @examplesIf interactive()
#' # List the available case studies
#' case_studies <- get_casestudies()
#' case_studies
#'
#' # Download a case study and load it into the R session
#' slick <- download_casestudy(case_studies$Name[1], case_studies)
#' slick
#'
#' # Save the case study file to the working directory instead
#' download_casestudy(case_studies$Name[1], case_studies, dir=getwd(),
#'                    object=FALSE, delete=FALSE)
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

#' @describeIn get_casestudies Download a case study and return the `Slick` object
#' @param name Character. The name of the case study to download, from the `Name`
#' column returned by `get_casestudies()`
#' @param case_studies Optional. The `data.frame` returned by `get_casestudies()`.
#' If not provided, `get_casestudies()` is called.
#' @param dir Optional. Directory to save the downloaded file. Defaults to [tempdir()]
#' @param silent Logical. Hide the download messages?
#' @param object Logical. Load and return the `Slick` object?
#' @param delete Logical. Delete the downloaded file when the function finishes?
#' Defaults to the value of `object`. Use `object=FALSE, delete=FALSE` to only save the file.
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


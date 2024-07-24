
#' Get Case Studies from the SlickLibrary GitHub Repo
#'
#' @return A data.frame
#' @export
#'
get_casestudies <- function() {
  req <- httr::GET("https://api.github.com/repos/blue-matter/slicklibrary/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  type <- unlist(lapply(httr::content(req)$tree, "[", "type"), use.names = F)
  filelist <- filelist[type=='blob']

  size <- unlist(lapply(httr::content(req)$tree, "[", "size"), use.names = F)

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
download_casestudy <- function(name, case_studies=NULL, dir=NULL, silent=FALSE, object=TRUE) {
  if (is.null(case_studies))
    case_studies <- get_casestudies()
  ind <- match(name, case_studies$Name)
  if (is.na(ind) | length(ind)<1) {
    stop(name, ' not found in Slick Library. Options are: ', paste(case_studies$Name, collapse=', '))
  }

  file <- case_studies$File[ind]
  url <- paste0('https://raw.githubusercontent.com/blue-matter/slicklibrary/master/', file)
  url <- gsub(' ', '%20', url)
  if (is.null(dir)) {
    dir <- getwd()
  }

  out_file <- file.path(dir, basename(file))
  if (!silent) cli::cli_alert_info(paste('Downloading', name, 'to', out_file))

  curl::curl_download(url,  out_file)

  download.file(url,  out_file,mode ="wb", quiet=FALSE)
  if (object)
    readRDS(out_file)
}


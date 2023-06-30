#' Download files of a Zenodo repository
#' 
#' @description
#' Downloads files of a Zenodo repository. If the repository is in restricted 
#' access, a token (argument `token`) must be provided.
#' 
#' @inheritParams zen_list_files
#' 
#' @param path a `character` of length 1. The folder in which files will be 
#'   saved.
#' 
#' @param files a `character` vector. If one or several files are provided, 
#'   only these files will be downloaded. If `NULL` (default) all files will be
#'   downloaded.
#'
#' @return No returned value.
#' 
#' @export
#' 
#' @examples
#' # List files available in the repo: https://zenodo.org/record/7936568 ----
#' zen_list_files("7936568")
#' 
#' \dontrun{
#' # Download one file ----
#' zen_download_files("7936568", files = "FORCIS_pump_v03_14062023.csv")
#' 
#' # Download all files ----
#' zen_download_files("7936568")
#' }

zen_download_files <- function(record_id, token, path = ".", files = NULL) {
  
  # Check args ----
  
  if (missing(record_id)) {
    stop("Argument 'record_id' is required", call. = FALSE)
  }
  
  if (!is.character(record_id) || length(record_id) != 1) {
    stop("Argument 'record_id' must be a character of length 1", call. = FALSE)
  }
  
  if (!missing(token)) {
    
    if (!is.character(token) || length(token) != 1) {
      stop("Argument 'token' must be a character of length 1", call. = FALSE)
    }
    
  } else {
    
    token <- ""
  }
  
  
  if (!is.character(path) || length(path) != 1) {
    stop("Argument 'path' must be a character of length 1", call. = FALSE)
  }
  
  if (!dir.exists(path)) {
    stop("The directory 'path' does not exist", call. = FALSE)
  }
  
  if (!is.null(files)) {
    if (!is.character(files)) {
      stop("Argument 'files' must be a character", call. = FALSE)
    }
  }
  
  
  # Build URLs ----
  
  user_raw_url <- full_raw_url(record_id, token)
  user_api_url <- full_api_url(record_id)
  
  
  # Generate cookies ----
  
  cookies  <- curl::new_handle()
  response <- curl::curl_fetch_memory(user_raw_url, handle = cookies)
  
  
  # Send request w/ cookies (~ token) ----
  
  response <- curl::curl_fetch_memory(user_api_url, handle = cookies)
  
  
  # Parse results ----
  
  metadata  <- jsonlite::fromJSON(rawToChar(response$"content"))
  
  
  # Check for token ----
  
  if (metadata$"metadata"$"access_right" == "restricted" && token == "") {
    stop(paste0("The repository is restricted. A token must be provided to ", 
                "access to the files.\nVisit <", user_raw_url, "> and request ",
                "access."), call. = FALSE)
  }
  
  
  # Get files metadata ----
  
  file_urls  <- metadata$"files"$"links"$"self"
  file_names <- basename(file_urls)
  
  
  # Select files ----
  
  if (!is.null(files)) {
    
    if (any(!(files %in% file_names))) {
      stop(paste0("Some files listed in 'files' are not present on the ", 
                  "repo.\nPlease run zen_list_files() to list available files"),
           call. = FALSE)
    }
    
    pos <- which(file_names %in% files)
    
    file_names <- file_names[pos]
    file_urls  <- file_urls[pos]
  }
  
  
  # Download files ----
  
  if (length(file_urls) > 0) {
    
    for (i in 1:length(file_urls)) {
      
      filename <- basename(file_urls[i])
      
      curl::curl_download(file_urls[i], file.path(path, filename), 
                          handle = cookies)
    }
  }
  
  invisible(NULL)
}

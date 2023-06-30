#' Base URL used to generate cookies
#' @noRd

raw_url <- function() "https://zenodo.org/record/"



#' Base URL used to retrieve metadata
#' @noRd

api_url <- function() "https://zenodo.org/api/records/"



#' Full URL used to generate cookies
#' @noRd

full_raw_url <- function(record_id, token) {
  
  if (token == "") {
    
    return(paste0(raw_url(), record_id))
    
  } else {
    
    return(paste0(raw_url(), record_id, "?token=", token) )
  }
}



#' Full URL used to retrieve metadata
#' @noRd

full_api_url <- function(record_id) {
  
  paste0(api_url(), record_id)
}

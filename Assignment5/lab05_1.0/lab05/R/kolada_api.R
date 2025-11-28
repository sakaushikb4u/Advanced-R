## ---------------------------
##
## Course: 732A94-Advanced R Programming
##
## Script name: LAB 05
##
## Date Created: 2023-10-06
##
## Copyright (c) MIT
## ---------------------------

## ---------------------------
## Util Functions Area
## ---------------------------
#' Get Json R object from a URL 
#'
#' @param url URL
#' @importFrom jsonlite fromJSON
#' @return Json object 
#' 
readJSONFromUrl <- function(url) {
  out <- tryCatch(
    {
      json_content <- jsonlite::fromJSON(url)
    },
    error=function(cond) {
      message(paste("URL not existed:", url))
      message("Error message:")
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message(paste("Warning:", url))
      message("warning message:")
      message(cond)
      return(NULL)
    },
    finally={
      message(paste("Processed URL:", url))
    }
  )    
  return(out)
}

#' Function to check if a string is a valid year
#'
#' @param input_string input year string
#' @return bool to show it's valid or not  
#' @export 
is_valid_year <- function(input_string) {
  # Use regular expression to match a 4-digit year  
  if (grepl("^(\\d{4},)*\\d{4}$", input_string)) {
      return(TRUE)
  }
  return(FALSE)
}

## ---------------------------
## Code Area
## ---------------------------
#' Get kolada municipality kpi groups  JSON object 
#'
#' @param search_title keywords to search in title field
#' @return kolada municipality kpi groups in data frame, if error happens, return NULL
#' @export 
kolada_municipality_kpi_groups <- function(search_title = ""){
  # Input check
  if(!(is.character(search_title)))
    stop("Illegal input_1!")
  
  # trim leading and tailing space
  search_title <- trimws(search_title,which = "both")
  
  # make request url
  reqURL_base <- "https://api.kolada.se/v2/kpi_groups"
  reqURL <- reqURL_base
  if (search_title != ""){
    reqURL <- paste(reqURL_base,"?title=",search_title,sep="")
  }
  
  # Get Json object from a url
  json_content <- readJSONFromUrl(reqURL)

  return(json_content)
 
}

#' Get kolada municipality information(id,title and type) in JSON object 
#'
#' @param search_title keywords to search in title field,optional
#' @return kolada municipality information in JSON format, if error happens, return NULL
#' @export 
kolada_municipality <- function(search_title = ""){
  # Input check
  if(!(is.character(search_title)))
    stop("Illegal input_2!")
  
  # trim leading and tailing space
  search_title <- trimws(search_title,which = "both")
  
  # make request url
  reqURL_base <- "https://api.kolada.se/v2/municipality"
  reqURL <- reqURL_base
  if (search_title != ""){
    reqURL <- paste(reqURL_base,"?title=",search_title,sep="")
  }
  
  # Get Json object from a url
  json_content <- readJSONFromUrl(reqURL)
  
  if (!is.na(json_content)[1] && !is.null(json_content)) {
    return(json_content)
  }else{
    return(list(count=0,values=data.frame()))
  }
}

#' Get kolada municipality kpi JSON object 
#'
#' @param kpi kpi to search
#' @param municipality city id to search
#' @param year years to search
#' @return kolada municipality kpis in JSON format, if error happens, return NULL
#' @export 
kolada_municipality_kpi <- function(kpi, municipality,year){
  # Input check
  if(!(is.character(kpi)))
    stop("Illegal input_3_1!")
  if(!(is.character(municipality)))
    stop("Illegal input_3_2!")
  if(!(is.character(year) && is_valid_year(year)))
    stop("Illegal input3_3_3")
  
  # trim leading and tailing space
  kpi <- trimws(kpi,which = "both")
  municipality <- trimws(municipality,which = "both")
  year <- trimws(year,which = "both")
  
  # make request url
  reqURL_base <- "https://api.kolada.se/v2/data"
  reqURL <- reqURL_base
  
  if (kpi == "") kpi = " "
  if (municipality == "") municipality = " "
  if (year == "") year = " "
  reqURL <- paste(reqURL_base,"kpi",kpi,"municipality",municipality,"year",year,sep="/")  
 
  # Get Json object from a URL
  json_content <- readJSONFromUrl(reqURL)
  
  # URL or network error will also make json_content na or null
  if (is.na(json_content)[1] || is.null(json_content)){
    return(NULL)
  }else {
    return(json_content)
  }
}
#'
#' @param output_format
#' @export

# devtools::document()

#'
#' @export

get_base_url <- function(){
  return("http://127.0.0.1:8000/clean/")
  # return("https://api-woyera.com/clean/")
}

#'
#' @export


clean <- function(api_key, clean_type, data){
  base_url <- get_base_url()
  full_url <- paste(base_url, clean_type, "/", sep="")

  r <- httr::POST(url=full_url, body=list(apiKey = api_key, data=as.list(data)), encode="json")
  post_status_code <- httr::status_code(r)

  if (post_status_code == 200){
    jsonResponse <- jsonlite::fromJSON(httr::content(r, "text"))
    clean_data_df <- data.frame(jsonResponse$cleanData)
    return(clean_data_df)
  }
  else{
    jsonResponse <- jsonlite::fromJSON(httr::content(r, "text"))
    if (typeof(jsonResponse) == "list"){
      if ("error" %in% names(jsonResponse)){
          stop(jsonResponse$error)
      }
      else{
        stop("There was an error. Try again")
      }
    }
    else{
      stop("There was an error. Try again")
    }

    print(jsonResponse)
  }
}



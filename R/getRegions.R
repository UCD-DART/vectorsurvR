#' @title Get region data
#' @description
#' `getSites()` obtains site data for authorized VectorSurv Gateway accounts.
#' @param token A valid access token returned from `getToken()`
#' @return A dataframe of region data, used internally to merge spatial information to collections


getRegions <- function(token) {
  if (is.null(token) | !is.character(token)) {
    stop("Invalid token. Check username and password")
  }


  headers <- c(Authorization = paste("Bearer", token),
               "Content-Type" = "application/json")


  url <- "https://api.vectorsurv.org/v1/region"

  #Gathers data matching parameters
  regions = data.frame()
  i = 1
  while (i > 0) {
    params <- list(pageSize = "1000", page = as.character(i))



    # Append the query string to the URL
    url_with_params <- modify_url(url, query = params)

    tryCatch({
      response <- GET(url_with_params, add_headers(headers))
      content <- content(response, as = "text")
      df_content = fromJSON(content, flatten = T)
      if (response$status_code != 200) {
        stop(content(response, 'parsed'))
      }
      #Breaks loop when df_content returns no more data
      if (length(df_content$rows) <= 0) {
        break
      }

      regions =  rbind(regions, df_content$rows)

    }, error = function(e) {
      stop(e)
    })

    i = i + 1
  }

  return(regions)

}

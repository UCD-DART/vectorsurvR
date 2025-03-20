#' @title Get Spatial data
#' @description
#' `getSpatialFeatures()` obtains spatial data for authorized VectorSurv Gateway accounts. Returns a list of all spatial features tied to an account.
#' @param token A valid access token returned from `getToken()`
#' @param agency_ids Filter on agency id, default to NULL for all available agencies,otherwise provide a vector of agency ids
#' @return A dataframe of region data, used internally to merge spatial information to collections
#' @export


getSpatialFeatures <- function(token, agency_ids = NULL) {
  if (is.null(token) | !is.character(token)) {
    stop("Invalid token. Check username and password")
  }


  headers <- c(Authorization = paste("Bearer", token),
               "Content-Type" = "application/json")

  if (!is.null(agency_ids) & length(agency_ids) > 1) {
    # If multiple agencies are provided, iterate over them, retrieve data, and merge
    spatial_list <- lapply(agency_ids, function(aid) {
      getSpatialFeatures(token, agency_ids = aid)
    })
    # Merge all the data together into a single dataframe
    merged_spatial <- bind_rows(spatial_list)
    return(merged_spatial)
  }

  url <- "https://api.vectorsurv.org/v1/geo-feature"

  #Gathers data matching parameters
  geofeat = data.frame()
  i = 1
  while (i > 0) {
    params <- list(pageSize = "1000",
                   page = as.character(i),
                   `query[agency][0]` = agency_ids)



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

      geofeat =  rbind(geofeat, df_content$rows)

    }, error = function(e) {
      stop(e)
    })

    i = i + 1
  }

  return(geofeat)

}

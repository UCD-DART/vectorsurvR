#' @title Get Spatial data
#' @description
#' `getSpatialFeatures()` obtains spatial data for authorized VectorSurv Gateway accounts. Returns a list of all spatial features tied to an account.
#' @param token A valid access token returned from `getToken()`
#' @param agency_ids Filter on agency id, default to NULL for all available agencies,otherwise provide a vector of agency ids
#' @return A dataframe of region data, used internally to merge spatial information to collections
#' @examples
#' \dontrun{token=getToken()
#' spatial = getSpatialFeatures(token)}
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



  # Function to convert coordinates to sf multipolygon
  convert_to_sf <- function(coords) {

    if (is.list(coords)) {
      coords <- coords[[1]]  # Extract if it's a list of one element
      longs <- coords[coords < 0]  # Values < 0 are longitudes
      lats <- coords[coords > 0]   # Values > 0 are latitudes


    }else{

      # Convert string of numbers into a numeric vector
      coords <- as.numeric(unlist(coords, recursive = TRUE))

      # Ensure there's an even number of values
      if (length(coords) %% 2 != 0) {
        warning("Odd number of coordinates detected, skipping this entry.")
        return(st_multipolygon())  # Return empty multipolygon
      }

      # Split into longitude and latitude
      num_points <- length(coords) / 2
      longs <- coords[1:num_points]
      lats  <- coords[(num_points + 1):length(coords)]

      # Ensure valid polygon (needs at least 3 points)
      if (length(longs) < 3 || length(lats) < 3) {
        warning("Skipping invalid polygon with fewer than 3 points.")
        return(st_multipolygon())
      }

      # Close the polygon if necessary
      if (!(longs[1] == longs[length(longs)] && lats[1] == lats[length(lats)])) {
        longs <- c(longs, longs[1])
        lats  <- c(lats, lats[1])
      }
    }
    # Create coordinate matrix
    coord_matrix <- cbind(longs, lats)

    # Create a multipolygon
    multipoly <- st_multipolygon(list(list(coord_matrix)))

    return(multipoly)
  }

  # Apply conversion
  if ("shape.coordinates" %in% colnames(geofeat)) {
    # Apply function to all shape coordinates

    geofeat$geometry <- lapply(geofeat$shape.coordinates, convert_to_sf)
    geofeat$geometry <- st_sfc(geofeat$geometry, crs = 4326) # Set CRS
    geofeat$geometry <- st_make_valid(geofeat$geometry)

    geofeat <- st_as_sf(geofeat)

  }







  return(geofeat)

}

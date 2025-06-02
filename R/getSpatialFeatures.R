#' Get Spatial Features data
#'
#' Retrieves VectorSurv spatial features data with optional agency filtering
#' @param token Access token retrieved from `getToken()`
#' @param agency_ids Optional vector of agency IDs to filter by
#' @return An sf object containing spatial features data
#' @export
#' @importFrom httr2 request req_headers req_perform resp_body_string req_url_query
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom sf st_multipolygon st_sfc st_as_sf st_make_valid
#' @examples
#' \dontrun{token=getToken()
#' spatial = getSpatialFeatures(token)}
#' @export

getSpatialFeatures <- function(token, agency_ids = NULL) {
  if (is.null(token) || !is.character(token)) {
    stop("Invalid token. Check username and password")
  }

  # Handle multiple agencies with recursion
  if (!is.null(agency_ids) && length(agency_ids) > 1) {
    spatial_list <- lapply(agency_ids, function(aid) {
      getSpatialFeatures(token, agency_ids = aid)
    })
    return(dplyr::bind_rows(spatial_list))
  }

  # Initialize empty results
  geofeat <- data.frame()
  page <- 1

  # Paginated API requests
  while(TRUE) {
    # Build and execute request
    req <- request("https://api.vectorsurv.org/v1/geo-feature") %>%
      req_headers(
        Authorization = paste("Bearer", token),
        "Content-Type" = "application/json"
      ) %>%
      req_url_query(
        pageSize = "1000",
        page = as.character(page),
        `query[agency][0]` = agency_ids
      )

    tryCatch({
      # Get and parse response
      response <- req_perform(req)
      content <- resp_body_string(response)
      df_content <- fromJSON(content, flatten = TRUE)

      # Break if no more results
      if (length(df_content$rows) <= 0) break

      # Append results
      geofeat <- dplyr::bind_rows(geofeat, df_content$rows)
      page <- page + 1

    }, error = function(e) {
      stop("API request failed: ", e$message)
    })
  }

  # Convert coordinates to sf multipolygon
  convert_to_sf <- function(coords) {
    if (is.list(coords)) {
      coords <- coords[[1]]  # Extract if it's a list of one element
      longs <- coords[coords < 0]  # Values < 0 are longitudes
      lats <- coords[coords > 0]   # Values > 0 are latitudes
    } else {
      # Convert string of numbers into a numeric vector
      coords <- as.numeric(unlist(coords, recursive = TRUE))

      # Ensure there's an even number of values
      if (length(coords) %% 2 != 0) {
        warning("Odd number of coordinates detected, skipping this entry.")
        return(sf::st_multipolygon())  # Return empty multipolygon
      }

      # Split into longitude and latitude
      num_points <- length(coords) / 2
      longs <- coords[1:num_points]
      lats <- coords[(num_points + 1):length(coords)]

      # Ensure valid polygon (needs at least 3 points)
      if (length(longs) < 3 || length(lats) < 3) {
        warning("Skipping invalid polygon with fewer than 3 points.")
        return(sf::st_multipolygon())
      }

      # Close the polygon if necessary
      if (!(longs[1] == longs[length(longs)] && lats[1] == lats[length(lats)])) {
        longs <- c(longs, longs[1])
        lats <- c(lats, lats[1])
      }
    }

    # Create coordinate matrix and multipolygon
    coord_matrix <- cbind(longs, lats)
    sf::st_multipolygon(list(list(coord_matrix)))
  }

  # Convert to sf object if shape coordinates exist
  if ("shape.coordinates" %in% colnames(geofeat)) {
    geofeat$geometry <- lapply(geofeat$shape.coordinates, convert_to_sf)
    geofeat$geometry <- sf::st_sfc(geofeat$geometry, crs = 4326)
    geofeat$geometry <- sf::st_make_valid(geofeat$geometry)
    geofeat <- sf::st_as_sf(geofeat)
  }

  return(geofeat)
}

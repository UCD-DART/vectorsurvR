#' Get Agency data
#'
#' Retrieves VectorSurv agency data with shape coordinates converted to sf geometry
#' @param token Access token retrieved from `getToken()`
#' @return An sf object containing agency data with geometry
#' @importFrom httr2 request req_headers req_perform resp_body_string req_url_query
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom sf st_multipolygon st_sfc st_as_sf st_make_valid
#' @examples
#' \dontrun{
#' token <- getToken()
#' agency <- getAgency(token)
#' }
#' @export
getAgency <- function(token) {
  if (is.null(token) || !is.character(token)) {
    stop("Invalid token. Check username and password")
  }

  # Initialize empty results
  agency <- data.frame()
  page <- 1

  # Paginated API requests
  while(TRUE) {
    # Build request with httr2
    req <- request("https://api.vectorsurv.org/v1/agency") %>%
      req_headers(
        Authorization = paste("Bearer", token),
        "Content-Type" = "application/json"
      ) %>%
      req_url_query(
        pageSize = "1000",
        page = as.character(page),
        `populate[]` = "shape"
      )

    # Execute request and process with jsonlite
    tryCatch({
      response <- req_perform(req)
      content <- resp_body_string(response)
      df_content <- fromJSON(content, flatten = TRUE)

      if (length(df_content$rows) <= 0) break

      agency <- bind_rows(agency, df_content$rows)
      page <- page + 1
    }, error = function(e) {
      stop("API request failed: ", e$message)
    })
  }

  # Convert coordinates to sf multipolygon (same function as getSpatialFeatures)
  convert_to_sf <- function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      return(sf::st_multipolygon())  # Return empty multipolygon
    }

    tryCatch({
      # Flatten any nested list structures and convert to numeric
      if (is.list(coords)) {
        coords <- unlist(coords, recursive = TRUE)
      }

      # Convert to numeric vector
      coords <- as.numeric(coords)

      # Remove any NAs
      coords <- coords[!is.na(coords)]

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

      # Create coordinate matrix and multipolygon
      coord_matrix <- cbind(longs, lats)
      sf::st_multipolygon(list(list(coord_matrix)))
    }, error = function(e) {
      warning("Error converting coordinates: ", e$message)
      return(sf::st_multipolygon())  # Return empty multipolygon on error
    })
  }

  # Convert to sf object if shape coordinates exist
  if ("shape.coordinates" %in% colnames(agency)) {
    agency$geometry <- lapply(agency$shape.coordinates, convert_to_sf)
    agency$geometry <- suppressWarnings(sf::st_sfc(agency$geometry, crs = 4326))
    agency$geometry <- suppressWarnings(sf::st_make_valid(agency$geometry))
    agency <- sf::st_as_sf(agency)
  }

  return(agency)
}

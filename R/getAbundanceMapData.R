#' @title Get Abundance Map Data
#' @description
#' `getAbundanceMapData()` retrieves spatial feature data and associates it with collection locations.
#' @param collections A dataframe containing collection coordinates
#' @param spatial A dataframe containing spatial data corresponding to the collections
#' @param interval Calculation interval for abundance, accepts“Biweek”,“Week”, and “Month
#' @param selected_features A character vector of spatial feature names to filter by
#' @return A dataframe of collections with associated spatial regions
#' @importFrom sf st_multipolygon st_sfc st_as_sf st_join st_crs st_transform st_make_valid st_cast st_is_valid st_within
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map map_chr
#' @example
#' \dontrun{token=getToken()
#' spatial = getSpatialFeatures(token)
#' collections = getArthrocollections(token, 2020,2021, 'mosquito')}
#'
#' @export
getAbundanceMapData <- function(collections, spatial, interval, selected_features = NULL) {

  #CHECK DATA FROM SAME SOURCES
  if(!all(unique(collections$agency_id) %in% unique(spatial$agency_id))){
    stop("Check spatial and collections data. Agency_ids do not match. Ensure same token is used to obtain both datasets")
  }


  # Retrieve spatial features
  spa <- spatial
  # Return available spatial features if none selected
  if (is.null(selected_features)) {
    selected_features <- unique(spa$name)
  }
  # Filter selected spatial features
  spa <- spa %>% dplyr::filter(name %in% selected_features)



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
  if ("shape.coordinates" %in% colnames(spa)) {
    # Apply function to all shape coordinates

    spa$geometry <- lapply(spa$shape.coordinates, convert_to_sf)
    spa$geometry <- st_sfc(spa$geometry, crs = 4326) # Set CRS
    spa$geometry <- st_make_valid(spa$geometry)

    spa_sf <- st_as_sf(spa)

  }
  else{
    spa_sf=spa
  }






  # Convert collections to sf object
  collections_sf <- st_as_sf(collections, coords = c("collection_longitude", "collection_latitude"), crs = 4326)

  # Ensure CRS consistency
  collections_sf <- st_transform(collections_sf, st_crs(spa_sf))

  # Spatial join to assign subregion names to collections
  collections_with_subregion <- st_join(collections_sf, spa_sf, join = st_within)

  # Add subregion name as a new column
  collections_with_subregion <- collections_with_subregion %>%
    dplyr::mutate(subregion = name) %>%
    dplyr::filter(!is.na(subregion))
  # Compute abundance
  abundance <- getAbundance(collections_with_subregion, interval, separate_by = c("subregion"))
  ab <- abundance %>%
    dplyr::group_by(Year, !!sym(interval), subregion) %>%
    dplyr::summarise(Abundance = mean(Abundance))
  colnames(ab)[3] <- "name"
  ab_sf <- spa_sf %>%
    dplyr::left_join(ab, by = "name")

  return(ab_sf)
}

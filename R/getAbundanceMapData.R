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
#'
#' @export
getAbundanceMapData <- function(collections,spatial,interval, selected_features) {

  #CHECK DATA FROM SAME SOURCES
  if(!all(unique(collections$agency_id) %in% unique(spatial$agency_id))){
    stop("Check spatial and collections data. Agency_ids do not match. Ensure same token is used to obtain both datasets")
  }


  # Retrieve spatial features
  spa_sf <- spatial
  # Return available spatial features if none selected
  if (is.null(selected_features)) {
    selected_features <- unique(spa_sf$name)
  }
  # Filter selected spatial features
  spa_sf <- spa_sf %>% dplyr::filter(name %in% selected_features)

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

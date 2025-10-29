#' Get Pools data
#'
#' Retrieves VectorSurv pools data for desired year range
#' @param token access token retrieved from `getToken()`
#' @param start_year Beginning of year range
#' @param end_year End of year range
#' @param arthropod Specify arthropod type from: 'mosquito', 'tick', 'nontick'
#' @param agency_ids Filter on agency id, default to NULL for all available agencies, otherwise provide a vector of agency ids
#' @keywords pools
#' @return Dataframe of pools data
#' @examples
#' \dontrun{
#' token = getToken()
#' getPools(token, start_year = 2020, end_year = 2021, arthropod = 'tick', 55)}
#' @export
#' @importFrom httr2 request req_headers req_perform resp_body_json req_url_query
#' @importFrom dplyr bind_rows inner_join select mutate if_else coalesce rename
#' @importFrom tidyr unnest
#' @importFrom lubridate year today


getPools <- function(token, start_year, end_year, arthropod, agency_ids = NULL) {

  valid_arthropods <- c("tick", "mosquito", "nontick")

  # Input validation
  if (!is.numeric(start_year) | !is.numeric(end_year)) {
    stop("Incorrect date format, start_year and end_year must be numeric")
  }
  if (end_year < start_year) {
    stop("Invalid year range, check parameters")
  }
  if (is.null(token) | !is.character(token)) {
    stop("Invalid token. Check username and password")
  }
  if (end_year > lubridate::year(lubridate::today())) {
    stop("Impossible year range. Check end_year")
  }
  if (!arthropod %in% valid_arthropods) {
    stop("Invalid arthropod type selected. Choose from: 'mosquito', 'tick', 'nontick'")
  }

  # Handle multiple agency_ids with recursion
  if (!is.null(agency_ids) && length(agency_ids) > 1) {
    pools_list <- lapply(agency_ids, function(aid) {
      getPools(token, start_year, end_year, arthropod, agency_ids = aid)
    })
    return(dplyr::bind_rows(pools_list))
  }

  # Initialize empty results
  pools <- data.frame()
  page <- 1

  # Paginated API requests
  while(TRUE) {
    # Build request with httr2
    req <- httr2::request("https://api.vectorsurv.org/v1/arthropod/pool") %>%
      httr2::req_headers(
        Authorization = paste("Bearer", token),
        "Content-Type" = "application/json"
      ) %>%
      httr2::req_url_query(
        type = arthropod,
        `populate[]` = "agency",
        `populate[]` = "test",
        `populate[]` = "status",
        `populate[]` = "trap",
        `populate[]` = "sex",
        `populate[]` = "lures",
        `populate[]` = "species",
        `populate[]` = "site",
        `populate[]` = "location",
        pageSize = "1000",
        page = as.character(page),
        `query[surv_year][$between][0]` = start_year,
        `query[surv_year][$between][1]` = end_year,
        `query[agency][0]` = agency_ids
      )

    # Execute request and process with jsonlite
    tryCatch({
      response <- httr2::req_perform(req)
      content <- httr2::resp_body_string(response)
      df_content <- jsonlite::fromJSON(content, flatten = TRUE)

      if (length(df_content$rows) <= 0) break
      pools <- dplyr::bind_rows(pools, df_content$rows)
      page <- page + 1
    }, error = function(e) {
      stop("API request failed: ", e$message)
    })
  }

  if (nrow(pools) <= 0) return(data.frame())

  # Data processing (unchanged from original)
  pools$test <- lapply(pools$test, as.data.frame)
  pools <- tidyr::unnest(pools, test, keep_empty = TRUE, names_sep = "_")

  colnames(pools) <- gsub("test_test_", "test_", colnames(pools)) %>%
    gsub("\\.", "_", .)

  # Extract coordinates
  coords <- do.call(rbind, lapply(pools$location_shape_coordinates, function(x) unlist(x)))
  pools$pool_longitude <- coords[,1]
  pools$pool_latitude <- coords[,2]

  # Join with spatial data
  sites <- getSites(token)
  sites_zip <- sites[c("id", "city", "postal_code", "region")]
  names(sites_zip)[1] <- "site_id"

  regions <- getRegions(token)
  regions_county <- regions[c("id", "parent", "type", "geoid", "namelsad")]
  names(regions_county)[1] <- "region"

  pools <- pools %>%
    dplyr::inner_join(sites_zip, by = "site_id") %>%
    dplyr::inner_join(regions_county, by = "region") %>%
    dplyr::mutate(
      namelsad = dplyr::if_else(
        !(.data$type %in% c("state", "county")),
        dplyr::coalesce(
          dplyr::left_join(., regions[c("id", "namelsad")], by = c("parent" = "id"))$namelsad.y,
          .data$namelsad
        ),
        .data$namelsad
      )
    )

  names(pools)[names(pools) == "namelsad"] <- "county"

  # Select columns based on arthropod type
  if (arthropod == "mosquito") {
    pools$lures <- lapply(pools$lures, as.data.frame)
    pools <- tidyr::unnest(pools, lures, keep_empty = TRUE, names_sep = "_")

    pools <- dplyr::select(
      pools,
      id, pool_num, agency_id, agency_code, agency_name, site_id, site_code, site_name,
      pool_longitude, pool_latitude, city, postal_code, county, geoid, collection, comments,
      surv_year, collection_date, species_display_name, species_full_name, sex_type, sex_name,
      trap_acronym, trap_name, trap_presence, lures_code, lures_description, lures_weight,
      num_count, test_value, test_date, test_method_name, test_method_acronym,
      test_target_acronym, test_target_vector, test_target_icd_10, test_status_name,
      test_agency_name, test_agency_code, test_agency_state_acronym, add_date, updated
    )

    pools <- pools %>%
      rename(pool_id = id)

  } else {
    pools <- dplyr::select(
      pools,
      pool_id, pool_num, agency_id, agency_code, agency_name, site_id, site_code, site_name,
      pool_longitude, pool_latitude, city, postal_code, county, geoid, collection, pool_comments,
      surv_year, collection_date, species_display_name, species_full_name, sex_type, sex_name,
      trap_acronym, trap_name, trap_presence, num_count, test_id, value, test_date,
      method_name, method_acronym, target_acronym, target_vector, target_icd_10, status_name,
      test_agency_name, test_agency_code, test_agency_state_acronym, add_date, updated
    )
  }

  return(pools)
}

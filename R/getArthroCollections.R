#' @title Get arthropod collections data
#' @description
#' `getArthroCollections()` obtains collections data on a year range [start_year, end_year] for authorized VectorSurv Gateway accounts.
#' @param token A valid access token returned from `getToken()`
#' @param start_year Start year of data
#' @param end_year  End year of data
#' @param arthropod Specify arthropod type from: 'mosquito', 'tick'
#' @param agency_ids Filter on agency id, default to NULL for all available agencies,otherwise provide a vector of agency ids, such as `agency_ids = c(55,56)`
#' @param spatial_features Filter data by spatial feature
#' @return A dataframe of collections data
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#' @importFrom stats setNames
#' @importFrom stringr str_replace str_replace_all
#' @importFrom httr2 req_method resp_status resp_body_string req_perform req_url_query
#' @importFrom sf st_intersects st_drop_geometry
#' @importFrom dplyr bind_rows select if_else pull coalesce inner_join left_join
#' @export
#' @examples
#' \dontrun{
#' token = getToken()
#' collections = getArthroCollections(token, 2021, 2022, 'mosquito',c(55,56), TRUE)}

getArthroCollections <- function(token, start_year, end_year, arthropod, agency_ids = NULL, spatial_features=NULL) {

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
    return(coord_matrix)
  }

  valid_arthropods <- c("tick", "mosquito")
  if (!(arthropod %in% valid_arthropods)) stop("Invalid arthropod type: choose 'mosquito' or 'tick'")
  if (!is.character(token) || is.null(token)) stop("Invalid token.")
  if (!is.numeric(start_year) || !is.numeric(end_year) || end_year < start_year) stop("Invalid year range.")

  if(!is.null(spatial_features)){
    spatial = getSpatialFeatures(token)

  }
  # Set base URL and populate parameters depending on arthropod type
  base_url <- switch(arthropod,
                     mosquito = "https://api.vectorsurv.org/v1/arthropod/collection",
                     tick = "https://api.vectorsurv.org/v1/tick/collection")

  populate_params <- switch(arthropod,
                            mosquito = c("arthropods", "agency", "trap", "lures", "location", "site"),
                            tick = c("agency", "ticks", "sample_method", "trap", "location", "site"))

  if (!is.null(agency_ids) && length(agency_ids) > 1) {
    return(bind_rows(lapply(agency_ids, function(aid) {
      getArthroCollections(token, start_year, end_year, arthropod, agency_ids = aid)
    })))
  }

  # Spatial features processing
  if(!is.null(spatial_features)) {
    spatial <- getSpatialFeatures(token)

    # Validate spatial features exist
    missing_features <- setdiff(spatial_features, spatial$name)
    if(length(missing_features) > 0) {
      warning("The following spatial features were not found: ",
              paste(missing_features, collapse = ", "))
      spatial_features <- setdiff(spatial_features, missing_features)
    }

    if(length(spatial_features) == 0) {
      stop("No valid spatial features provided")
    }

    shapes <- spatial %>% filter(name %in% spatial_features)

    # Check for overlaps between features
    overlaps <- sf::st_intersects(shapes, sparse = FALSE)
    if(any(overlaps[lower.tri(overlaps)])) {
      overlapping_pairs <- which(overlaps, arr.ind = TRUE) %>%
        as.data.frame() %>%
        filter(row < col) %>%
        mutate(pair = paste(spatial_features[row], "&", spatial_features[col]))
      warning("The following spatial features overlap:\n",
              paste(unique(overlapping_pairs$pair), collapse = "\n"))
    }

    # If only one spatial feature, process normally
    if(length(spatial_features) == 1) {
      shape <- shapes[1,]
      coords <- convert_to_sf(shape$shape.coordinates)
      coord_list <- lapply(1:nrow(coords), function(i) unname(as.numeric(coords[i, ])))
      geojson_coords <- list(list(coord_list))

      geojson_payload <- list(
        geojson = list(
          type = "Feature",
          geometry = list(
            type = "MultiPolygon",
            coordinates = geojson_coords
          )
        ),
        distance = list(max = 0)
      )

      # Make single request with spatial filter
      all_data <- list()
      i <- 1
      repeat {
        req <- request(base_url) %>% req_method("GET") %>%
          req_headers(
            Authorization = paste("Bearer", token),
            `Content-Type` = "application/json"
          ) %>%
          req_url_query(
            !!!setNames(populate_params, rep("populate[]", length(populate_params))),
            pageSize = "1000",
            page = as.character(i),
            `query[surv_year][$between][0]` = start_year,
            `query[surv_year][$between][1]` = end_year,
            `query[agency][0]` = if (!is.null(agency_ids)) agency_ids else NULL
          ) %>%
          req_body_json(geojson_payload)

        resp <- req_perform(req)
        if (resp_status(resp) != 200) {
          stop("Failed request: ", resp_body_string(resp))
        }

        raw_json <- resp_body_string(resp)
        df <- jsonlite::fromJSON(raw_json, flatten = TRUE)
        if(length(df$rows) == 0) break

        all_data[[i]] <- df$rows
        i <- i + 1
      }

      if(length(all_data) == 0) return(data.frame())
      collections <- bind_rows(all_data)

    }
    else {
      # For multiple features, make separate requests
      all_results <- list()

      for(feature in spatial_features) {
        feature_data <- list()
        i <- 1

        shape <- shapes %>% filter(name == feature)
        coords <- convert_to_sf(shape$shape.coordinates)
        coord_list <- lapply(1:nrow(coords), function(i) unname(as.numeric(coords[i, ])))
        geojson_coords <- list(list(coord_list))

        geojson_payload <- list(
          geojson = list(
            type = "Feature",
            geometry = list(
              type = "MultiPolygon",
              coordinates = geojson_coords
            )
          ),
          distance = list(max = 0)
        )

        repeat {
          req <- request(base_url) %>% req_method("GET") %>%
            req_headers(
              Authorization = paste("Bearer", token),
              `Content-Type` = "application/json"
            ) %>%
            req_url_query(
              !!!setNames(populate_params, rep("populate[]", length(populate_params))),
              pageSize = "1000",
              page = as.character(i),
              `query[surv_year][$between][0]` = start_year,
              `query[surv_year][$between][1]` = end_year,
              `query[agency][0]` = if (!is.null(agency_ids)) agency_ids else NULL
            ) %>%
            req_body_json(geojson_payload)

          resp <- req_perform(req)
          if (resp_status(resp) != 200) {
            warning("Failed request for spatial feature: ", feature)
            break
          }

          raw_json <- resp_body_string(resp)
          df <- jsonlite::fromJSON(raw_json, flatten = TRUE)
          if(length(df$rows) == 0) break

          feature_data[[i]] <- df$rows
          i <- i + 1
        }

        if(length(feature_data) > 0) {
          all_results[[feature]] <- bind_rows(feature_data)
        }
      }

      if(length(all_results) == 0) return(data.frame())
      collections <- bind_rows(all_results)

    }

    collections$collection_longitude <- sapply(collections$location.shape.coordinates, function(x) unlist(x)[1])
    collections$collection_latitude <- sapply(collections$location.shape.coordinates, function(x) unlist(x)[2])

    # Convert collections to sf object for spatial join
    collections_sf <- sf::st_as_sf(
      collections,
      coords = c("collection_longitude", "collection_latitude"),
      crs = 4326
    )

    # Get the spatial features we filtered by
    filtered_shapes <- spatial %>%
      filter(name %in% spatial_features) %>%
      select(name)

    # Perform spatial join to identify which features contain each point
    spatial_matches <- sf::st_join(collections_sf, filtered_shapes) %>%
      st_drop_geometry()  # Convert back to regular dataframe

    # Join the spatial feature info back to original collections
    collections <- collections %>%
      left_join(
        spatial_matches %>%
          group_by(id) %>%
          summarize(
            spatial_feature = paste(unique(na.omit(name)), collapse = ", "),
            multiple_features = n_distinct(name) > 1
          ),
        by = "id"
      )

    # Handle points not in any spatial feature
    collections <- collections %>%
      mutate(
        spatial_feature = ifelse(is.na(spatial_feature), NA_character_, spatial_feature),
        multiple_features = ifelse(is.na(multiple_features), FALSE, multiple_features)
      )
  } else {
    # Non-spatial filtered request
    all_data <- list()
    i <- 1

    repeat {
      req <- request(base_url) %>% req_method("GET") %>%
        req_headers(
          Authorization = paste("Bearer", token),
          `Content-Type` = "application/json"
        ) %>%
        req_url_query(
          !!!setNames(populate_params, rep("populate[]", length(populate_params))),
          pageSize = "1000",
          page = as.character(i),
          `query[surv_year][$between][0]` = start_year,
          `query[surv_year][$between][1]` = end_year,
          `query[agency][0]` = if (!is.null(agency_ids)) agency_ids else NULL
        )

      resp <- req_perform(req)
      if (resp_status(resp) != 200) {
        stop("Failed request: ", resp_body_string(resp))
      }

      raw_json <- resp_body_string(resp)
      df <- jsonlite::fromJSON(raw_json, flatten = TRUE)
      if(length(df$rows) == 0) break

      all_data[[i]] <- df$rows
      i <- i + 1
    }

    if(length(all_data) == 0) return(data.frame())
    collections <- bind_rows(all_data)
  }

  if (arthropod == "mosquito") {
    collections$arthropods <- lapply(collections$arthropods, as.data.frame)

    collections <- collections %>%
      unnest(arthropods, keep_empty = TRUE, names_sep = "_")
    collections$lures <- lapply(collections$lures, as.data.frame)

    collections <- collections %>%
      unnest(lures, keep_empty = TRUE, names_sep = "_")


    colnames(collections) =  str_replace(colnames(collections), "arthropods_","")

    names(collections) <- str_replace_all(names(collections), "\\.", "_")
    colnames(collections)[1] = 'collection_id'
    collections = collections %>%  dplyr::filter(!(species_display_name%in%c("V pensylvanica","D variabilis" ,"D occidentalis","I pacificus","Dermacentor","V germanica")))
    collections$collection_longitude <- sapply(collections$location_shape_coordinates, function(x) unlist(x)[1])
    collections$collection_latitude <- sapply(collections$location_shape_coordinates, function(x) unlist(x)[2])
  }

  if (arthropod == "tick") {
    collections$ticks <- lapply(collections$ticks, as.data.frame)

    collections <- collections %>%
      unnest(ticks, keep_empty = TRUE, names_sep = "_")
    colnames(collections) =  str_replace(colnames(collections), "ticks_","")

    names(collections) <- str_replace_all(names(collections), "\\.", "_")
    colnames(collections)[1] = 'collection_id'
    colnames(collections)[which(names(collections) == "type")] <- "collection_type"
    collections$collection_id <- collections$id
    collections$collection_longitude <- sapply(collections$location_shape_coordinates, function(x) unlist(x)[1])
    collections$collection_latitude <- sapply(collections$location_shape_coordinates, function(x) unlist(x)[2])
  }

  # Join site and region data (uses assumed external helper functions getSites and getRegions)




  sites = getSites(token)
  sites_zip = sites[c("id", "city", "postal_code", "region")] #selects the columns with relevant information, this can be changed of course
  regions = getRegions(token)
  regions_county = regions[c("id","parent","type","geoid", "namelsad")] #select id and county name
  colnames(regions_county)[1] = "region" #rename for join function
  colnames(regions_county)[which(names(regions_county) == "type")] <- "region_type"

  colnames(sites_zip)[1] = "site_id" #rename for join function

  col_site = left_join(collections, sites_zip, by = 'site_id') #join site information to collections
  region_site = left_join(sites_zip,regions_county, by="region")
  collections = left_join(collections, region_site, by = "site_id")

  collections=collections %>%
    mutate(namelsad = if_else(!(region_type %in% c("state","county")),
                              # For geoid > 5, join and update 'namelsad'
                              left_join(., regions %>% dplyr::select(id, namelsad), by = c("parent" = "id")) %>%
                                mutate(namelsad = coalesce(namelsad.y, namelsad.x)) %>%
                                pull(namelsad),
                              # For geoid <= 5, keep the original 'namelsad'
                              namelsad))

  colnames(collections)[which(names(collections) == "namelsad")] <- "county"


  if(arthropod == "mosquito") {
    #remove unwanted/redundant columns

    if(!("lures_code"%in% colnames(collections))){
      collections$lures_code = as.character(NA)
      collections$lures_description =as.character(NA)
      collections$lures_id =NA
      collections$lures_weight = NA
    }
      base_cols <- c(
        "collection_id", "collection_num", "collection_date",
        "agency_id", "agency_code", "agency_name", "surv_year",
        "comments", "identified_by", "species_display_name",
        "sex_name", "sex_type", "trap_acronym", "lures_id",
        "lures_code", "lures_description", "lures_weight", "num_trap",
        "trap_nights", "trap_problem_bit", "num_count",
        "site_id", "site_code", "site_name", "collection_longitude",
        "collection_latitude", "city", "postal_code", "county", "geoid",
        "add_date", "deactive_date", "updated"
      )

  }
  if(arthropod == "tick") {
    base_cols <- c(
      "collection_id", "collection_num", "collection_date_start",
      "collection_date_end", "agency_id", "agency_code", "agency_name",
      "surv_year", "comments", "identified_by", "species_display_name",
      "sex_name", "sex_type", "trap_acronym", "bloodfed", "attached",
      "num_count", "trap_problem_bit", "sample_method_name",
      "sample_method_value", "host", "humidity", "wind_speed",
      "temperature", "conditions_moisture", "conditions_sunlight",
      "site_id", "site_code", "site_name", "collection_longitude",
      "collection_latitude", "city", "postal_code", "county", "geoid",
      "add_date", "deactive_date", "updated"
    )
}
    if(!is.null(spatial_features)) {
      collections <- collections %>%
        select(all_of(base_cols),spatial_feature, multiple_features)
    } else {
      collections <- collections %>%
        select(all_of(base_cols))
    }

  return(collections)
}


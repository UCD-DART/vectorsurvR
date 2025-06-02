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
#' @importFrom httr2 req_method resp_status resp_body_string req_perform
#' @importFrom sf st_intersects
#' @importFrom dplyr bind_rows select if_else pull coalesce inner_join left_join
#' @export
#' @examples
#' \dontrun{
#' token = getToken()
#' collections = getArthroCollections(token, 2021, 2022, 'mosquito',c(55,56), TRUE)}

getArthroCollections <- function(token, start_year, end_year, arthropod, agency_ids = NULL, spatial_features=NULL) {


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

  all_data <- list()
  i <- 1

  repeat {
    req <- request(base_url)  |>  req_method("GET") |>
      req_headers(
        Authorization = paste("Bearer", token),
        `Content-Type` = "application/json"
      ) |>
      req_url_query(
        !!!setNames(populate_params, rep("populate[]", length(populate_params))),
        pageSize = "1000",
        page = as.character(i),
        `query[surv_year][$between][0]` = start_year,
        `query[surv_year][$between][1]` = end_year,
        `query[agency][0]` = if (!is.null(agency_ids)) agency_ids else NULL
      )

    #spatial feature filter
    # Spatial feature filter - updated for multiple features
    if(!is.null(spatial_features)){

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
      spatial = getSpatialFeatures(token)

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

      # Get all matching shapes
      shapes <- spatial %>%
        filter(name %in% spatial_features)

      if(nrow(shapes) == 0) {
        stop("No matching spatial features found")
      }

      # Process all shapes into single MultiPolygon
      all_coords <- list()

      for(i in seq_len(nrow(shapes))) {
        shape <- shapes[i,]
        coords <- convert_to_sf(shape$shape.coordinates)

        # Convert coordinates to correct format
        coord_list <- lapply(1:nrow(coords), function(i) {
          unname(as.numeric(coords[i, ]))
        })

        # Add to our collection of polygons
        all_coords <- c(all_coords, list(coord_list))
      }

      # Create single MultiPolygon with all features
      geojson_coords <- list(all_coords)

      # Build payload
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

      req <- req |>
        req_body_json(geojson_payload)

      # Check for overlapping features if multiple were provided
      if(length(spatial_features) > 1) {
        overlaps <- st_intersects(shapes, sparse = FALSE)
        if(any(overlaps[lower.tri(overlaps)])) {
          overlapping_pairs <- which(overlaps, arr.ind = TRUE) %>%
            as.data.frame() %>%
            filter(row < col) %>%
            mutate(pair = paste(spatial_features[row], "&", spatial_features[col]))
          warning("The following spatial features overlap:\n",
                  paste(unique(overlapping_pairs$pair), collapse = "\n"))
        }
      }
    }
    resp <- req_perform(req)

    if (resp_status(resp) != 200) {
      stop("Failed request: ", resp_body_string(resp))
    }
    raw_json <- resp_body_string(resp)  # get raw JSON text

    # Parse JSON and flatten nested fields automatically:
    df <- jsonlite::fromJSON(raw_json, flatten = TRUE)
    rows<-df$rows




    if (length(rows) == 0) break
    all_data[[i]] <- rows
    i <- i + 1
  }

  if (length(all_data) == 0) return(data.frame())

  collections <- bind_rows(all_data)

  if (arthropod == "mosquito") {
    collections$arthropods <- lapply(collections$arthropods, as.data.frame)
    collections$lures <- lapply(collections$lures, as.data.frame)

    collections <- collections %>%
      unnest(arthropods, keep_empty = TRUE, names_sep = "_") |>
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

    collections <- collections |>
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
  colnames(sites_zip)[1] = "site_id" #rename for join function

  col_site = left_join(collections, sites_zip, by = 'site_id') #join site information to collections
  regions_county = regions[c("id","parent","type","geoid", "namelsad")] #select id and county name
  colnames(regions_county)[1] = "region" #rename for join function
  colnames(regions_county)[which(names(regions_county) == "type")] <- "region_type"

  collections = left_join(col_site, regions_county, by = "region")

  collections=collections %>%
    mutate(namelsad = if_else(!(region_type %in% c("state","county")),
                              # For geoid > 5, join and update 'namelsad'
                              left_join(., regions %>% dplyr::select(id, namelsad), by = c("parent" = "id")) %>%
                                mutate(namelsad = coalesce(namelsad.y, namelsad.x)) %>%
                                pull(namelsad),
                              # For geoid <= 5, keep the original 'namelsad'
                              namelsad))


  colnames(collections)[which(names(collections) == "namelsad")] <- "county"


  if(arthropod=="mosquito"){
    #remove unwanted/redundant columns
    collections = collections %>%
      select(collection_id,collection_num, collection_date,
             agency_id, agency_code, agency_name, surv_year,
             comments,identified_by,species_display_name,
             sex_name,sex_type,trap_acronym,lures_id, lures_code, lures_description, lures_weight,num_trap,
             trap_nights,trap_problem_bit,num_count,
             site_id, site_code, site_name,collection_longitude,collection_latitude,city,postal_code, county,geoid, add_date,
             deactive_date, updated)
  }
  if(arthropod=="tick"){
    collections = collections %>%
      select(collection_id,collection_num, collection_date_start,collection_date_end,
             agency_id, agency_code, agency_name, surv_year,
             comments,identified_by,species_display_name,
             sex_name,sex_type,trap_acronym,bloodfed, attached, num_count,trap_problem_bit,sample_method_name,sample_method_value,host,humidity,wind_speed,temperature,conditions_moisture,conditions_sunlight,
             site_id, site_code, site_name,collection_longitude,collection_latitude,city,postal_code, county,geoid, add_date,
             deactive_date, updated)
  }

  collections <- collections %>%
    select(where(~ !all(is.na(.)))) # Drop all-NA columns

  return(collections)
}

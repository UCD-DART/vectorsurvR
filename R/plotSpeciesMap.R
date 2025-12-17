#'@title Plot Species Map
#'@description Plot map of mosquito species
#'@param token x
#'@param target_year x
#'@param target_species x
#'@param trap_type x
#'@param agency_ids
#'@param interval
#'@param basemap
#'@param show_heatmap
#'@param output_file
#'@param output_format
#'@param width
#'@param height

#'@title Plot Species Map (FULL VERSION)
plotSpeciesMap <- function(token, target_year, target_species = NULL,
                           trap_type = NULL, agency_ids = NULL,
                           time_period = NULL, interval = "Month",
                           basemap = "Topographic", spatial_features = NULL,
                           show_heatmap = FALSE, output_file = NULL,
                           output_format = "leaflet", width = 1200, height = 800) {

  library(leaflet)
  library(dplyr)
  library(tidyr)
  library(sf)

  # Get spatial features if requested
  spatial_filtered <- NULL
  if(!is.null(spatial_features)){
    spatial <- getSpatialFeatures(token, agency_ids = agency_ids)

    if(!is.null(spatial) && nrow(spatial) > 0) {
      spatial_filtered <- spatial %>% filter(name %in% spatial_features)

      if(nrow(spatial_filtered) > 0 && inherits(spatial_filtered, "sf")) {

        # Ensure valid
        if(is.na(st_crs(spatial_filtered))) st_crs(spatial_filtered) <- 4326
        spatial_filtered <- st_make_valid(spatial_filtered)
      } else {
        cat("No valid spatial features found\n")
        spatial_filtered <- NULL
      }
    }
    cat("================================\n\n")
  }

  # Get sites
  sites <- getSites(token, agency_ids = agency_ids)
  sites$site_longitude <- sapply(sites$shape.coordinates, function(x) if(!is.null(x) && length(x) > 0) unlist(x)[1] else NA)
  sites$site_latitude <- sapply(sites$shape.coordinates, function(x) if(!is.null(x) && length(x) > 0) unlist(x)[2] else NA)

  # Get collection data
  data_full <- getArthroCollections(
    token,
    start_year = target_year,
    end_year = target_year,
    arthropod = "mosquito",
    agency_ids = agency_ids,
    geocoded = FALSE
  )

  # Check if data was retrieved
  if (is.null(data_full) || nrow(data_full) == 0) {
    warning("No data retrieved")
    return(NULL)
  }

  # Filter data
  data <- data_full

  if (!is.null(target_species)) {
    data <- data %>% filter(species_display_name %in% target_species)
  }

  if (!is.null(trap_type)) {
    data <- data %>% filter(trap_acronym %in% trap_type)
  }

  data <- data %>% filter(!is.na(collection_longitude) & !is.na(collection_latitude))

  if (nrow(data) == 0) {
    warning("No data with valid coordinates after filtering")
    return(NULL)
  }

  # Process dates and time periods
  if (!inherits(data$collection_date, "Date")) {
    data$collection_date <- as.Date(data$collection_date)
  }

  data <- data %>%
    mutate(
      week = as.integer(format(collection_date, "%U")),
      biweek = as.integer(format(collection_date, "%U")) %/% 2 + 1,
      month = as.integer(format(collection_date, "%m")),
      month_name = format(collection_date, "%B"),
      time_period_value = case_when(
        interval == "Week" ~ paste0("Week ", week),
        interval == "Biweek" ~ paste0("Biweek ", biweek),
        interval == "Month" ~ month_name
      )
    )

  if (!is.null(time_period)) {
    if (is.numeric(time_period)) {
      month_names <- c("January", "February", "March", "April", "May", "June",
                       "July", "August", "September", "October", "November", "December")
      if (time_period >= 1 && time_period <= 12) {
        time_period_name <- month_names[time_period]
        data <- data %>% filter(month == time_period | month_name == time_period_name)
      }
    } else {
      data <- data %>% filter(time_period_value == time_period)
    }
  }

  # Process sites
  active_site_coords <- data_full %>%
    distinct(collection_longitude, collection_latitude) %>%
    filter(!is.na(collection_longitude) & !is.na(collection_latitude)) %>%
    mutate(is_active = TRUE)

  sites <- sites %>%
    left_join(active_site_coords,
              by = c("site_longitude" = "collection_longitude",
                     "site_latitude" = "collection_latitude")) %>%
    mutate(is_active = ifelse(is.na(is_active), FALSE, TRUE))

  year_sites <- sites %>%
    filter(code %in% unique(data_full$site_code[!is.na(data_full$site_code)]))

  active_sites <- year_sites %>% filter(is_active == TRUE)
  inactive_sites <- year_sites %>% filter(is_active == FALSE)

  # Summarize data for mapping
  map_data <- data %>%
    group_by(collection_longitude, collection_latitude, species_display_name) %>%
    summarise(
      species_count = sum(num_count, na.rm = TRUE),
      species_collections = n(),
      county = first(county),
      trap_acronym = first(trap_acronym),
      .groups = "drop"
    ) %>%
    group_by(collection_longitude, collection_latitude) %>%
    mutate(
      total_count = sum(species_count),
      n_species = n_distinct(species_display_name)
    ) %>%
    ungroup()

  cat("\n=== BUILDING MAP ===\n")

  # Create base map WITHOUT spatial features first
  m <- leaflet() %>%
    {
      if (basemap == "Satellite") {
        addProviderTiles(., providers$Esri.WorldImagery)
      } else if (basemap == "Topographic") {
        addProviderTiles(., providers$Esri.WorldTopoMap)
      } else if (basemap == "Terrain") {
        addProviderTiles(., providers$Esri.WorldTerrain)
      } else {
        addProviderTiles(., providers$Esri.WorldTopoMap)
      }
    }

  cat("Base map created\n")

  # Add inactive sites
  if (nrow(inactive_sites) > 0) {
    m <- m %>%
      addCircleMarkers(
        data = inactive_sites,
        lng = ~site_longitude, lat = ~site_latitude,
        radius = 2,
        color = "darkgrey",
        fillColor = "lightgrey",
        fillOpacity = 0.4,
        popup = ~paste("<b>Site:</b>", name),
        group = "Monitoring Sites"
      )
    cat("Added", nrow(inactive_sites), "inactive sites\n")
  }

  # Add active sites
  if (nrow(active_sites) > 0) {
    m <- m %>%
      addCircleMarkers(
        data = active_sites,
        lng = ~site_longitude, lat = ~site_latitude,
        radius = 3,
        color = "white",
        fillColor = "black",
        fillOpacity = 0.8,
        popup = ~paste("<b>Site:</b>", name),
        group = "Monitoring Sites"
      )
    cat("Added", nrow(active_sites), "active sites\n")
  }

  # Add collection data
  if (nrow(map_data) > 0) {
    all_species <- unique(map_data$species_display_name)
    species_colors <- setNames(viridis::plasma(length(all_species)), all_species)

    location_summary <- map_data %>%
      group_by(collection_longitude, collection_latitude) %>%
      summarise(n_species = first(n_species), .groups = "drop")

    single_species_locs <- location_summary %>% filter(n_species == 1)

    if (nrow(single_species_locs) > 0) {
      single_data <- map_data %>%
        semi_join(single_species_locs, by = c("collection_longitude", "collection_latitude"))

      species_palette <- colorFactor(species_colors, domain = all_species)

      m <- m %>%
        addCircleMarkers(
          data = single_data,
          lng = ~collection_longitude,
          lat = ~collection_latitude,
          radius = ~sqrt(species_count) * 2,
          color = "white",
          fillColor = ~species_palette(species_display_name),
          fillOpacity = 0.8,
          popup = ~paste("<b>Species:</b>", species_display_name),
          group = "Collections"
        )
      cat("Added", nrow(single_data), "collection markers\n")
    }

    if (length(all_species) > 0) {
      m <- m %>%
        addLegend(
          position = "bottomright",
          colors = species_colors,
          labels = names(species_colors),
          title = "Species",
          opacity = 1
        )
    }
  }

  # NOW add spatial features LAST
  if(!is.null(spatial_filtered) && nrow(spatial_filtered) > 0) {
    cat("Adding spatial features...\n")

    tryCatch({
      m <- m %>%
        addPolygons(
          data = spatial_filtered,
          fillColor = "#0066CC",
          color = "#0066CC",
          weight = 2,
          opacity = 0.8,
          fillOpacity = 0.15,
          popup = ~paste("<b>Area:</b>", name),
          group = "Spatial Features"
        )
      cat("Spatial features added successfully!\n")
    }, error = function(e) {
      cat("ERROR adding spatial features:", e$message, "\n")
    })
  }

  # Add title and controls
  title_text <- paste0(
    "<div style='background:white;padding:8px;border-radius:4px;'>",
    "<b>Mosquito Collections - ", target_year, "</b>",
    "</div>"
  )

  m <- m %>%
    addControl(html = title_text, position = "topright") %>%
    addScaleBar(position = "bottomleft")

  # Add layers control
  groups <- c("Monitoring Sites", "Collections")
  if(!is.null(spatial_filtered) && nrow(spatial_filtered) > 0) {
    groups <- c(groups, "Spatial Features")
  }

  m <- m %>%
    addLayersControl(
      overlayGroups = groups,
      options = layersControlOptions(collapsed = FALSE)
    )

  # Set bounds based on data (NOT spatial features)
  m <- m %>%
    fitBounds(
      min(data$collection_longitude, na.rm = TRUE),
      min(data$collection_latitude, na.rm = TRUE),
      max(data$collection_longitude, na.rm = TRUE),
      max(data$collection_latitude, na.rm = TRUE)
    )

  cat("Map complete!\n")
  cat("====================\n\n")

  suppressWarnings(return(m))
}

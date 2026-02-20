#' @title Create Interactive Map of Mosquito Species Collection Data
#'
#' @description Generates an interactive Leaflet map visualizing mosquito species collection data and monitoring sites
#' from surveillance activities. Displays collection locations colored by species found in that location during the indicated time period, with the option to include spatial features
#'
#' @param token Authentication token for API access retrieved from getToken()
#' @param target_year Target surveillance year for data retrieval
#' @param species Optional vector of species names to filter results. View  with `unique(data$species_display_name)`
#' @param trap Optional vector of trap acronyms to filter results. View available
#'                 trap types with `unique(data$trap_acronym)`
#' @param agency_ids Optional vector of agency IDs to filter results when access to multiple agencies
#' @param interval Time interval to select time period from: "Week", "Biweek", "Month" or NULL. If NULL data is for entire year.
#' @param time_period Optional time period to filter results within the designated interval. Can be numeric
#'  (1-12 for months) or character (e.g., "July", "Week 5")
#' @param basemap Base map design: "Topographic", "Satellite", or "Terrain"
#' @param spatial_features Optional vector of spatial feature names to display as polygons. View available features using `getSpatialFeatures()`
#' @param width Map width in pixels for saved output (numeric, default = 1200)
#' @param height Map height in pixels for saved output (numeric, default = 800)
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers addPolygons addLegend addControl addScaleBar addLayersControl layersControlOptions fitBounds colorFactor addMarkers makeIcon
#' @importFrom dplyr filter mutate group_by summarise ungroup left_join semi_join distinct first row_number n
#' @importFrom sf st_crs<- st_make_valid st_as_sf st_sf st_polygon st_sfc st_crs
#' @importFrom viridis plasma
#' @importFrom lubridate epiweek
#'
#' @return Returns a map object visualizing mosquito species collection data. Returns
#'         NULL if no data is available after filtering.
#'
#' @export
plotSpeciesMap <- function(token, target_year, species = NULL,
                           trap = NULL, agency_ids = NULL, interval = NULL,
                           time_period = NULL, basemap = "Topographic", spatial_features = NULL,
                           width = 1200, height = 800) {

  # Helper function to create SVG pie chart
  create_pie_svg <- function(values, colors, radius = 20) {
    total <- sum(values)
    if (total == 0) return("")

    percentages <- values / total
    cumsum_perc <- c(0, cumsum(percentages))

    # Create SVG paths for each slice
    paths <- character(length(values))
    for (i in seq_along(values)) {
      start_angle <- cumsum_perc[i] * 2 * pi
      end_angle <- cumsum_perc[i + 1] * 2 * pi

      x1 <- radius + radius * cos(start_angle - pi/2)
      y1 <- radius + radius * sin(start_angle - pi/2)
      x2 <- radius + radius * cos(end_angle - pi/2)
      y2 <- radius + radius * sin(end_angle - pi/2)

      large_arc <- ifelse(percentages[i] > 0.5, 1, 0)

      paths[i] <- sprintf(
        '<path d="M %f %f L %f %f A %f %f 0 %d 1 %f %f Z" fill="%s" stroke="white" stroke-width="1"/>',
        radius, radius, x1, y1, radius, radius, large_arc, x2, y2, colors[i]
      )
    }

    svg <- sprintf(
      '<svg width="%.0f" height="%.0f" xmlns="http://www.w3.org/2000/svg">%s</svg>',
      radius * 2, radius * 2, paste(paths, collapse = "")
    )

    # Convert to data URI
    paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(svg)))
  }

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

  # Get agency boundaries - always load and make toggleable
  agency <- NULL
  agency_result <- getAgency(token)

  if(!is.null(agency_result) && nrow(agency_result) > 0) {
    # Filter to requested agencies if agency_ids provided
    if(!is.null(agency_ids)) {
      agency <- agency_result %>% filter(id %in% agency_ids)
    } else {
      agency <- agency_result
    }

    # Validate as sf object (same as spatial features)
    if(nrow(agency) > 0 && inherits(agency, "sf")) {
      # Ensure valid CRS and geometry
      if(is.na(st_crs(agency))) st_crs(agency) <- 4326
      agency <- st_make_valid(agency)
      cat("Loaded", nrow(agency), "agency boundaries\n")
    } else {
      cat("Agency data is not in sf format\n")
      agency <- NULL
    }
  } else {
    cat("No agency boundaries available\n")
    agency <- NULL
  }
  cat("================================\n\n")

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

  if (!is.null(species)) {
    data <- data %>% filter(species_display_name %in% species)
  }

  if (!is.null(trap)) {
    data <- data %>% filter(trap_acronym %in% trap)
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
      week = as.integer(epiweek(collection_date)),
      biweek = ceiling(as.integer(epiweek(collection_date))/2),
      month = as.integer(format(collection_date, "%m")),
      month_name = format(collection_date, "%B")
    )

  # Apply time period filtering based on interval type
  if (!is.null(interval) && !is.null(time_period)) {
    if (interval == "Week") {
      data <- data %>% filter(week %in% time_period)
    } else if (interval == "Biweek") {
      data <- data %>% filter(biweek %in% time_period)
    } else if (interval == "Month") {
      if (is.numeric(time_period)) {
        data <- data %>% filter(month %in% time_period)
      } else {
        data <- data %>% filter(month_name %in% time_period)
      }
    }
  }

  # Check if data remains after time filtering
  if (nrow(data) == 0) {
    warning("No data available for the specified time period")
    return(NULL)
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

  # Create base map
  m <- leaflet() %>%
    {
      if (basemap == "Satellite") {
        addProviderTiles(., "Esri.WorldImagery")
      } else if (basemap == "Topographic") {
        addProviderTiles(., "Esri.WorldTopoMap")
      } else if (basemap == "Terrain") {
        addProviderTiles(., "Esri.WorldTerrain")
      } else {
        addProviderTiles(., "Esri.WorldTopoMap")
      }
    }

  cat("Base map created\n")

  # Add agency boundaries FIRST
  if(!is.null(agency) && nrow(agency) > 0) {
    cat("Adding agency boundaries...\n")
    tryCatch({
      m <- m %>%
        addPolygons(
          data = agency,
          fillColor = "transparent",
          color = "grey",
          weight = 3,
          opacity = 0.9,
          fillOpacity = 0,
          dashArray = "5, 5",
          popup = ~paste("<b>Agency:</b>", name),
          group = "Agency Boundaries"
        )
      cat("Agency boundaries added successfully!\n")
    }, error = function(e) {
      cat("ERROR adding agency boundaries:", e$message, "\n")
    })
  }


  # Add spatial features
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



  # Add collection data with pie charts for multi-species locations
  if (nrow(map_data) > 0) {
    all_species <- sort(unique(map_data$species_display_name))

    # Create color palette
    if (length(all_species) <= 5) {
      color_indices <- round(seq(1, 256, length.out = length(all_species)))
      species_colors <- setNames(viridis::plasma(256)[color_indices], all_species)
    } else {
      species_colors <- setNames(viridis::plasma(length(all_species)), all_species)
    }

    # Separate single vs multi-species locations
    location_summary <- map_data %>%
      group_by(collection_longitude, collection_latitude) %>%
      summarise(n_species = first(n_species), .groups = "drop")

    single_species_locs <- location_summary %>% filter(n_species == 1)
    multi_species_locs <- location_summary %>% filter(n_species > 1)

    # Add single-species locations as circle markers
    if (nrow(single_species_locs) > 0) {
      single_data <- map_data %>%
        semi_join(single_species_locs, by = c("collection_longitude", "collection_latitude"))

      species_palette <- leaflet::colorFactor(species_colors, domain = all_species)

      m <- m %>%
        addCircleMarkers(
          data = single_data,
          lng = ~collection_longitude,
          lat = ~collection_latitude,
          radius = ~sqrt(species_count) /2,
          color = "white",
          fillColor = ~species_palette(species_display_name),
          fillOpacity = 0.8,
          popup = ~paste0(
            "<b>Species:</b> ", species_display_name, "<br>",
            "<b>Count:</b> ", species_count, "<br>",
            "<b>Collections:</b> ", species_collections
          ),
          group = "Collections"
        )
      cat("Added", nrow(single_data), "single-species markers\n")
    }

    # Add multi-species locations as pie chart markers using SVG icons
    if (nrow(multi_species_locs) > 0) {
      multi_data <- map_data %>%
        semi_join(multi_species_locs, by = c("collection_longitude", "collection_latitude"))

      unique_multi_locs <- multi_data %>%
        distinct(collection_longitude, collection_latitude)

      for (i in 1:nrow(unique_multi_locs)) {
        loc_data <- multi_data %>%
          filter(collection_longitude == unique_multi_locs$collection_longitude[i],
                 collection_latitude == unique_multi_locs$collection_latitude[i])

        total_count <- sum(loc_data$species_count)
        radius <- sqrt(total_count) /2  # Reduced from 2 to 1 to make smaller

        values <- loc_data$species_count
        colors <- species_colors[loc_data$species_display_name]

        popup_text <- paste0(
          "<b>Total Count:</b> ", total_count, "<br>",
          "<b>Species:</b><br>",
          paste("&nbsp;&nbsp;", loc_data$species_display_name, ": ", loc_data$species_count, collapse = "<br>")
        )

        # Create pie chart icon
        pie_icon <- leaflet::makeIcon(
          iconUrl = create_pie_svg(values, colors, radius),
          iconWidth = radius * 2,
          iconHeight = radius * 2,
          iconAnchorX = radius,
          iconAnchorY = radius
        )

        m <- m %>%
          addMarkers(
            lng = unique_multi_locs$collection_longitude[i],
            lat = unique_multi_locs$collection_latitude[i],
            icon = pie_icon,
            popup = popup_text,
            group = "Collections"
          )
      }
      cat("Added", nrow(unique_multi_locs), "pie chart markers\n")
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
    # Add legend
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


  # Build dynamic title
  title_parts <- c(paste0("Mosquito Collections - ", target_year))

  if (!is.null(interval) && !is.null(time_period)) {
    if (interval == "Month") {
      period_labels <- if(is.numeric(time_period)) month.name[time_period] else time_period
      if (length(period_labels) == 1) {
        title_parts <- c(title_parts, period_labels)
      } else if (length(period_labels) <= 3) {
        title_parts <- c(title_parts, paste(period_labels, collapse = ", "))
      } else {
        title_parts <- c(title_parts, paste0(period_labels[1], " - ", period_labels[length(period_labels)]))
      }
    } else if (interval == "Week") {
      if (length(time_period) == 1) {
        title_parts <- c(title_parts, paste0("Week ", time_period))
      } else if (length(time_period) <= 4) {
        title_parts <- c(title_parts, paste0("Weeks ", paste(time_period, collapse = ", ")))
      } else {
        title_parts <- c(title_parts, paste0("Weeks ", min(time_period), "-", max(time_period)))
      }
    } else if (interval == "Biweek") {
      if (length(time_period) == 1) {
        title_parts <- c(title_parts, paste0("Biweek ", time_period))
      } else if (length(time_period) <= 4) {
        title_parts <- c(title_parts, paste0("Biweeks ", paste(time_period, collapse = ", ")))
      } else {
        title_parts <- c(title_parts, paste0("Biweeks ", min(time_period), "-", max(time_period)))
      }
    }
  }

  if (!is.null(species)) {
    if (length(species) <= 3) {
      title_parts <- c(title_parts, paste0("Species: ", paste(species, collapse = ", ")))
    } else {
      title_parts <- c(title_parts, paste0(length(species), " species"))
    }
  }

  title_text <- paste0(
    "<div style='background:white;padding:8px;border-radius:4px;'>",
    "<b>", paste(title_parts, collapse = " | "), "</b>",
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
  if(!is.null(agency) && nrow(agency) > 0) {
    groups <- c(groups, "Agency Boundaries")
  }

  m <- m %>%
    addLayersControl(
      overlayGroups = groups,
      options = layersControlOptions(collapsed = FALSE)
    )

  # Set bounds
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

#' @title Plot Species Map
#' @description Plot map of mosquito species
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

plotSpeciesMap <- function(token, target_year, target_species = NULL,
                           trap_type = NULL, agency_ids = NULL,
                           time_period = NULL, interval = "Month",
                           basemap = "Topographic", show_heatmap = FALSE,
                           output_file = NULL, output_format = "leaflet",
                           width = 1200, height = 800) {


  # Get ALL sites (unfiltered)
  sites <- getSites(token, agency_ids = agency_ids)

  sites$site_longitude <- sapply(sites$shape.coordinates, function(x) unlist(x)[1])
  sites$site_latitude <- sapply(sites$shape.coordinates, function(x) unlist(x)[2])

  # Get the data
  data_full <- getArthroCollections(token, target_year, target_year,
                                    arthropod = "mosquito", agency_ids = agency_ids, geocoded = F)

  # Filter data
  if (!is.null(target_species)) {
    data <- data_full %>% filter(species_display_name %in% target_species)
  }

  if (!is.null(trap_type)) {
    data <- data %>% filter(trap_acronym %in% trap_type)
  }

  # Remove rows with missing coordinates
  data <- data %>%
    filter(!is.na(collection_longitude) & !is.na(collection_latitude))

  # Add temporal grouping
  data <- data %>%
    mutate(
      collection_date = as.Date(collection_date),
      week = as.integer(format(collection_date, "%U")),
      biweek = as.integer(format(collection_date, "%U")) %/% 2,
      month = as.integer(format(collection_date, "%m")),
      month_name = format(collection_date, "%B"),
      time_period_value = case_when(
        interval == "Week" ~ paste0("Week ", week),
        interval == "Biweek" ~ paste0("Biweek ", biweek),
        interval == "Month" ~ month_name
      )
    )

  # Filter to specific time period if requested
  if (!is.null(time_period)) {
    data <- data %>% filter(time_period_value == time_period)
  }

  # Get active sites for this time period (sites with ANY collections, even if 0 mosquitoes)
  active_site_coords <- data %>%
    distinct(collection_longitude, collection_latitude) %>%
    mutate(is_active = TRUE)

  # Mark which sites are active
  sites <- sites %>%
    left_join(
      active_site_coords,
      by = c("site_longitude" = "collection_longitude",
             "site_latitude" = "collection_latitude")
    ) %>%
    mutate(is_active = ifelse(is.na(is_active), FALSE, is_active))

  # Separate active and inactive sites
  year_sites = sites[sites$code%in%unique(data_full$site_code),]
  active_sites <- year_sites %>% filter(is_active == TRUE)
  inactive_sites <- year_sites %>% filter(is_active == FALSE)

  # Summarize data - keep track of species at each location
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

  # Create base map with selected basemap
  m <- leaflet() %>%
    {
      if (basemap == "Satellite") {
        addProviderTiles(., providers$Esri.WorldImagery, group = "Satellite")
      } else if (basemap == "Topographic") {
        addProviderTiles(., providers$Esri.WorldTopoMap, group = "Topographic")
      } else if (basemap == "Terrain") {
        addProviderTiles(., providers$Esri.WorldTerrain, group = "Terrain")
      } else if (basemap == "OpenTopoMap") {
        addProviderTiles(., providers$OpenTopoMap, group = "OpenTopoMap")
      } else {
        addProviderTiles(., providers$Esri.WorldImagery, group = "Satellite")
      }
    } %>%

    # Add inactive monitoring sites for that year (grey)
    addCircleMarkers(
      data = inactive_sites,
      lng = ~site_longitude, lat = ~site_latitude,
      radius = 2,
      color = "darkgrey",
      fillColor = "lightgrey",
      fillOpacity = 0.4,
      stroke = TRUE,
      weight = 1,
      popup = ~paste("<b>Site:</b>", name, "<br>",
                     "<b>Status:</b> No collections this period<br>",
                     "<b>Lat:</b>", round(site_latitude, 4), "<br>",
                     "<b>Lon:</b>", round(site_longitude, 4)),
      group = "Monitoring Sites"
    ) %>%

    # Add active monitoring sites (highlighted - black with white border)
    addCircleMarkers(
      data = active_sites,
      lng = ~site_longitude, lat = ~site_latitude,
      radius = 3,
      color = "white",
      fillColor = "black",
      fillOpacity = 0.8,
      stroke = TRUE,
      weight = 2,
      popup = ~paste("<b>Site:</b>", name, "<br>",
                     "<b>Status:</b> Active this period<br>",
                     "<b>Lat:</b>", round(site_latitude, 4), "<br>",
                     "<b>Lon:</b>", round(site_longitude, 4)),
      group = "Monitoring Sites"
    )

  # Add collection data (consider adding for abundance?)
  if (nrow(map_data) > 0) {
    if (show_heatmap) {
      # Aggregate for heatmap
      heatmap_data <- map_data %>%
        group_by(collection_longitude, collection_latitude) %>%
        summarise(total_count = first(total_count), .groups = "drop")

      m <- m %>%
        addHeatmap(
          data = heatmap_data,
          lng = ~collection_longitude,
          lat = ~collection_latitude,
          intensity = ~total_count,
          blur = 20,
          max = 0.8,
          radius = 15,
          group = "Collections"
        )
    } else {
      # Get all unique species for consistent colors
      all_species <- unique(map_data$species_display_name)
      species_colors <- setNames(
        viridis::plasma(length(all_species)),
        all_species
      )

      # Separate single vs multi-species locations
      location_summary <- map_data %>%
        group_by(collection_longitude, collection_latitude) %>%
        summarise(
          n_species = first(n_species),
          total_count = first(total_count),
          .groups = "drop"
        )

      single_species_locs <- location_summary %>% filter(n_species == 1)
      multi_species_locs <- location_summary %>% filter(n_species > 1)

      # Add circle markers for single-species locations
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
            stroke = TRUE,
            weight = 1.5,
            popup = ~paste("<b>Species:</b>", species_display_name, "<br>",
                           "<b>Count:</b>", species_count, "<br>",
                           "<b>Collections:</b>", species_collections, "<br>",
                           "<b>County:</b>", county, "<br>",
                           "<b>Trap:</b>", trap_acronym),
            group = "Collections"
          )
      }

      # Add pie charts for multi-species locations
      if (nrow(multi_species_locs) > 0) {
        # Prepare wide format for minicharts
        pie_wide <- map_data %>%
          semi_join(multi_species_locs, by = c("collection_longitude", "collection_latitude")) %>%
          select(collection_longitude, collection_latitude, species_display_name, species_count, total_count) %>%
          pivot_wider(
            id_cols = c(collection_longitude, collection_latitude, total_count),
            names_from = species_display_name,
            values_from = species_count,
            values_fill = 0
          )

        # Only proceed if we have valid data
        if (nrow(pie_wide) > 0) {
          # Get species columns in correct order
          species_cols <- intersect(all_species, names(pie_wide))

          # Make sure we have at least 2 species columns (otherwise it's not really multi-species)
          if (length(species_cols) >= 2) {
            chartdata <- as.matrix(pie_wide[, species_cols])

            # Ensure chartdata is numeric matrix
            if (!is.numeric(chartdata)) {
              chartdata <- apply(chartdata, 2, as.numeric)
            }

            # Get colors as unnamed vector in same order as species_cols
            pie_colors <- unname(species_colors[species_cols])

            # Add pie charts
            m <- m %>%
              addMinicharts(
                lng = pie_wide$collection_longitude,
                lat = pie_wide$collection_latitude,
                type = "pie",
                chartdata = chartdata,
                colorPalette = pie_colors,
                width = 45 * sqrt(pie_wide$total_count / max(pie_wide$total_count)),
                transitionTime = 0,
                opacity = 0.9,
                layerId = paste0("pie_", 1:nrow(pie_wide))
              )
          }
        }
      }

      # Add legend
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

  # Add title and scale
  title_text <- if (!is.null(time_period)) {
    paste0("<div style='background:white;padding:8px;border-radius:4px;box-shadow:0 2px 4px rgba(0,0,0,0.3);'>",
           "<b>Mosquito Collections - ", time_period, " ", target_year, "</b>",
           if (!is.null(target_species)) paste0("<br><i>", paste(target_species, collapse=", "), "</i>") else "",
           "</div>")
  } else {
    paste0("<div style='background:white;padding:8px;border-radius:4px;box-shadow:0 2px 4px rgba(0,0,0,0.3);'>",
           "<b>Mosquito Collections - ", target_year, "</b>",
           if (!is.null(target_species)) paste0("<br><i>", paste(target_species, collapse=", "), "</i>") else "",
           "</div>")
  }

  # Calculate bounds for map view based on collection locations
  all_lons <- data$collection_longitude
  all_lats <- data$collection_latitude

  # Remove NAs
  all_lons <- all_lons[!is.na(all_lons)]
  all_lats <- all_lats[!is.na(all_lats)]

  m <- m %>%
    addControl(html = title_text, position = "topright") %>%
    addScaleBar(position = "bottomleft") %>%
    addLayersControl(
      overlayGroups = c("Monitoring Sites", "Collections"),
      options = layersControlOptions(collapsed = FALSE)
    )

  # Only add fitBounds if we have valid coordinates
  if (length(all_lons) > 0 && length(all_lats) > 0) {
    m <- m %>%
      fitBounds(
        lng1 = min(all_lons),
        lat1 = min(all_lats),
        lng2 = max(all_lons),
        lat2 = max(all_lats)
      )
  }

  # Handle output format
  if (output_format == "pdf" || output_format == "png") {
    # Generate filename if not provided
    if (is.null(output_file)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      period_str <- if (!is.null(time_period)) paste0("_", gsub(" ", "_", time_period)) else ""
      output_file <- paste0("map_", target_year, period_str, "_", timestamp,
                            if (output_format == "pdf") ".pdf" else ".png")
    }

    # Create webshot
    webshot_result <- mapshot(m, file = output_file,
                              remove_controls = c("zoomControl", "homeButton"),
                              vwidth = width, vheight = height)
    message(paste("Map saved to:", output_file))
    return(webshot_result)

  } else if (!is.null(output_file)) {
    # Save file but still return leaflet object
    mapshot(m, file = output_file,
            remove_controls = c("zoomControl", "homeButton"),
            vwidth = width, vheight = height)
    message(paste("Map saved to:", output_file))
    return(m)

  } else {
    # Return interactive leaflet map
    return(m)
  }
}

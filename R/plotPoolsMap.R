#' @title Create Interactive Map of Mosquito Pool Testing Data
#'
#' @description Generates an interactive Leaflet map visualizing mosquito pool testing results
#' by location, species, and test outcome. The map displays positive and negative pools,
#' monitoring sites, and optional spatial features with filtering capabilities.
#'
#' @param token Authentication token for API access (character)
#' @param target_year Target surveillance year for data retrieval (numeric)
#' @param target_disease Optional vector of disease acronyms to filter results (character)
#' @param species Optional vector of species names to filter results (character)
#' @param trap Optional vector of trap acronyms to filter results (character)
#' @param agency_ids Optional vector of agency IDs to filter results (character)
#' @param interval Time interval for grouping data: "Week", "Biweek", "Month", or "Year" (character, default = "Month")
#' @param time_period Optional time period to filter results within the year (character)
#' @param basemap Base map type: "Topographic", "Satellite", "Terrain", or "OpenTopoMap" (character, default = "Topographic")
#' @param spatial_features Optional vector of spatial feature names to display (character)
#' @param show_agency_boundaries Whether to display agency boundaries (logical, default = TRUE)
#' @param html Logical. TRUE (default) returns an interactive leaflet widget for HTML output.
#'   Set to FALSE when knitting to Word or PDF to return a static screenshot instead.
#' @param height Pixel height of the screenshot when html = FALSE. Default is 400.
#' @return A leaflet map object (html = TRUE) or a static image (html = FALSE).
#' @export
#' @importFrom leaflet leaflet addProviderTiles addPolygons addCircleMarkers addControl
#' @importFrom leaflet addScaleBar addLayersControl fitBounds addLegend
#' @importFrom leaflet.minicharts addMinicharts
#' @importFrom dplyr filter group_by summarise mutate distinct left_join semi_join
#' @importFrom dplyr arrange ungroup case_when
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate epiweek month
#' @importFrom sf st_as_sf
#' @importFrom viridis plasma
#'
#' @examples
#' \dontrun{
#' # Basic map for a target year
#' plotPoolsMap(token, target_year = 2023)
#'
#' # Filter by disease and time period
#' plotPoolsMap(
#'   token          = token,
#'   target_year    = 2023,
#'   target_disease = "WNV",
#'   interval       = "Month",
#'   time_period    = "July"
#' )
#'
#' # With spatial features and satellite basemap
#' plotPoolsMap(
#'   token            = token,
#'   target_year      = 2023,
#'   agency_ids       = 55,
#'   spatial_features = c("Zone3", "Downtown"),
#'   basemap          = "Satellite"
#' )
#'
#' # Static output for Word or PDF reports
#' plotPoolsMap(
#'   token          = token,
#'   target_year    = 2023,
#'   target_disease = "WNV",
#'   html           = FALSE,
#'   height         = 500
#' )
#' }
plotPoolsMap <- function(token, target_year, target_disease = NULL,
                         species = NULL, trap = NULL,
                         agency_ids = NULL, time_period = NULL,
                         interval = "Month", basemap = "Topographic",
                         spatial_features = NULL, show_agency_boundaries = TRUE,
                         html = TRUE, height = 400) {

  # Get spatial features if requested
  spatial_filtered <- NULL
  if (!is.null(spatial_features)) {
    spatial <- getSpatialFeatures(token, agency_ids = agency_ids)

    if (!is.null(spatial) && nrow(spatial) > 0) {
      spatial_filtered <- spatial %>% filter(name %in% spatial_features)

      if (nrow(spatial_filtered) > 0) {
        cat("Loaded", nrow(spatial_filtered), "spatial features\n")
      } else {
        cat("No matching spatial features found\n")
        spatial_filtered <- NULL
      }
    }
    cat("================================\n\n")
  }

  # Get agency boundaries
  agency <- NULL
  if (show_agency_boundaries == TRUE) {
    agency <- getAgency(token)
    agency <- agency %>% filter(id %in% agency_ids)
    if (!is.null(agency) && nrow(agency) > 0) {
      cat("Loaded", nrow(agency), "agency boundaries\n")
    } else {
      cat("No agency boundaries available\n")
      agency <- NULL
    }
  }
  cat("================================\n\n")

  # Get sites
  sites <- getSites(token, agency_ids = agency_ids)
  sites$site_longitude <- sapply(sites$shape.coordinates, function(x) unlist(x)[1])
  sites$site_latitude  <- sapply(sites$shape.coordinates, function(x) unlist(x)[2])

  # Get pools data
  data_full <- getPools(token, target_year, target_year, arthropod = "mosquito", agency_ids = agency_ids)

  if (is.null(data_full) || nrow(data_full) == 0) {
    warning("No pools data retrieved")
    return(NULL)
  }

  cat("Retrieved", nrow(data_full), "pool records\n")

  # Filter data
  data <- data_full

  if (!is.null(target_disease)) {
    data <- data %>% filter(test_target_acronym %in% target_disease)
    cat("Filtered to disease(s):", paste(target_disease, collapse = ", "), "\n")
  }

  if (!is.null(species)) {
    data <- data %>% filter(species_display_name %in% species)
    cat("Filtered to species:", paste(species, collapse = ", "), "\n")
  }

  if (!is.null(trap)) {
    data <- data %>% filter(trap_acronym %in% trap)
    cat("Filtered to trap(s):", paste(trap, collapse = ", "), "\n")
  }

  # Remove rows with missing coordinates
  data <- data %>% filter(!is.na(pool_longitude) & !is.na(pool_latitude))

  if (nrow(data) == 0) {
    warning("No data with valid coordinates after filtering")
    return(NULL)
  }

  # Add temporal grouping columns
  data <- data %>%
    mutate(
      collection_date   = as.Date(collection_date),
      week              = as.integer(epiweek(collection_date)),
      biweek            = as.integer(epiweek(collection_date)) %/% 2,
      month             = as.integer(format(collection_date, "%m")),
      month_name        = format(collection_date, "%B"),
      year              = format(collection_date, "%Y"),
      time_period_value = case_when(
        interval == "Week"   ~ paste0("Week ", week),
        interval == "Biweek" ~ paste0("Biweek ", biweek),
        interval == "Month"  ~ month_name,
        interval == "Year"   ~ year,
        TRUE                 ~ year
      ),
      test_result = case_when(
        tolower(test_status_name) == "confirmed" ~ "Positive",
        tolower(test_status_name) == "negative"  ~ "Negative",
        tolower(test_status_name) == "pending"   ~ "Pending"
      )
    )

  # Filter to specific time period if requested
  if (!is.null(time_period) && interval != "Year") {
    data <- data %>% filter(time_period_value == time_period)
    cat("Filtered to time period:", time_period, "\n")
  }

  if (nrow(data) == 0) {
    warning("No data available for the specified time period")
    return(NULL)
  }

  cat("================================\n\n")

  # Mark active vs inactive sites
  active_site_coords <- data %>%
    distinct(pool_longitude, pool_latitude) %>%
    mutate(is_active = TRUE)

  sites <- sites %>%
    left_join(
      active_site_coords,
      by = c("site_longitude" = "pool_longitude", "site_latitude" = "pool_latitude")
    ) %>%
    mutate(is_active = ifelse(is.na(is_active), FALSE, is_active))

  year_sites    <- sites[sites$code %in% unique(data_full$site_code), ]
  active_sites  <- year_sites %>% filter(is_active == TRUE)
  inactive_sites <- year_sites %>% filter(is_active == FALSE)

  # Summarize data by location and test result
  map_data <- data %>%
    group_by(pool_longitude, pool_latitude, species_display_name, test_result, test_target_acronym) %>%
    summarise(
      pool_count      = n_distinct(id),
      mosquito_count  = sum(num_count, na.rm = TRUE),
      county          = first(county),
      trap_acronym    = first(trap_acronym),
      .groups         = "drop"
    )

  positive_pools <- map_data %>% filter(test_result == "Positive")
  negative_pools <- map_data %>% filter(test_result == "Negative")

  cat("=== BUILDING MAP ===\n")

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

  # Add agency boundaries
  if (!is.null(agency) && nrow(agency) > 0) {
    cat("Adding agency boundaries...\n")
    for (i in 1:nrow(agency)) {
      coords <- agency$shape.coordinates[[i]]

      if (!is.null(coords) && length(coords) > 0) {
        tryCatch({
          if (is.array(coords) && !is.list(coords)) {
            coords_dim <- dim(coords)
            for (poly_idx in 1:coords_dim[1]) {
              for (ring_idx in 1:coords_dim[2]) {
                coords_matrix <- coords[poly_idx, ring_idx, , ]
                if (is.matrix(coords_matrix) && ncol(coords_matrix) >= 2) {
                  lngs <- coords_matrix[, 1]
                  lats <- coords_matrix[, 2]
                  if (length(lngs) > 0 && !all(is.na(lngs))) {
                    m <- m %>%
                      addPolygons(
                        lng = lngs, lat = lats,
                        fillColor = "transparent", color = "#666666",
                        weight = 2, opacity = 0.6, fillOpacity = 0,
                        popup = paste("<b>Agency:</b>", agency$name[i]),
                        label = agency$name[i],
                        group = "Agency Boundaries"
                      )
                  }
                }
              }
            }
          } else if (is.list(coords)) {
            for (poly_idx in 1:length(coords)) {
              poly <- coords[[poly_idx]]
              if (is.array(poly)) {
                poly_dim <- dim(poly)
                if (length(poly_dim) == 3 && poly_dim[1] == 1 && poly_dim[3] == 2) {
                  coords_matrix <- poly[1, , ]
                  if (is.matrix(coords_matrix) && ncol(coords_matrix) >= 2) {
                    lngs <- coords_matrix[, 1]
                    lats <- coords_matrix[, 2]
                    if (length(lngs) > 0 && !all(is.na(lngs))) {
                      m <- m %>%
                        addPolygons(
                          lng = lngs, lat = lats,
                          fillColor = "transparent", color = "#666666",
                          weight = 2, opacity = 0.6, fillOpacity = 0,
                          popup = paste("<b>Agency:</b>", agency$name[i]),
                          label = agency$name[i],
                          group = "Agency Boundaries"
                        )
                    }
                  }
                }
              } else if (is.matrix(poly) && ncol(poly) >= 2) {
                lngs <- poly[, 1]
                lats <- poly[, 2]
                if (length(lngs) > 0 && !all(is.na(lngs))) {
                  m <- m %>%
                    addPolygons(
                      lng = lngs, lat = lats,
                      fillColor = "transparent", color = "#666666",
                      weight = 2, opacity = 0.6, fillOpacity = 0,
                      popup = paste("<b>Agency:</b>", agency$name[i]),
                      label = agency$name[i],
                      group = "Agency Boundaries"
                    )
                }
              }
            }
          }
        }, error = function(e) {
          cat("ERROR adding agency boundary:", e$message, "\n")
        })
      }
    }
    cat("Agency boundaries added successfully!\n")
  }

  # Add spatial features
  if (!is.null(spatial_filtered) && nrow(spatial_filtered) > 0) {
    cat("Adding spatial features...\n")
    for (i in 1:nrow(spatial_filtered)) {
      coords <- spatial_filtered$shape.coordinates[[i]]

      if (!is.null(coords) && length(coords) > 0) {
        tryCatch({
          if (is.array(coords) && !is.list(coords)) {
            coords_dim <- dim(coords)
            for (poly_idx in 1:coords_dim[1]) {
              for (ring_idx in 1:coords_dim[2]) {
                coords_matrix <- coords[poly_idx, ring_idx, , ]
                if (is.matrix(coords_matrix) && ncol(coords_matrix) >= 2) {
                  lngs <- coords_matrix[, 1]
                  lats <- coords_matrix[, 2]
                  if (length(lngs) > 0 && !all(is.na(lngs))) {
                    m <- m %>%
                      addPolygons(
                        lng = lngs, lat = lats,
                        fillColor = "#0066CC", color = "#0066CC",
                        weight = 2, opacity = 0.8, fillOpacity = 0.15,
                        popup = paste("<b>Area:</b>", spatial_filtered$name[i]),
                        label = spatial_filtered$name[i],
                        group = "Spatial Features"
                      )
                  }
                }
              }
            }
          } else if (is.list(coords)) {
            for (poly_idx in 1:length(coords)) {
              poly <- coords[[poly_idx]]
              if (is.array(poly)) {
                poly_dim <- dim(poly)
                if (length(poly_dim) == 3 && poly_dim[1] == 1 && poly_dim[3] == 2) {
                  coords_matrix <- poly[1, , ]
                  if (is.matrix(coords_matrix) && ncol(coords_matrix) >= 2) {
                    lngs <- coords_matrix[, 1]
                    lats <- coords_matrix[, 2]
                    if (length(lngs) > 0 && !all(is.na(lngs))) {
                      m <- m %>%
                        addPolygons(
                          lng = lngs, lat = lats,
                          fillColor = "#0066CC", color = "#0066CC",
                          weight = 2, opacity = 0.8, fillOpacity = 0.15,
                          popup = paste("<b>Area:</b>", spatial_filtered$name[i]),
                          label = spatial_filtered$name[i],
                          group = "Spatial Features"
                        )
                    }
                  }
                }
              } else if (is.matrix(poly) && ncol(poly) >= 2) {
                lngs <- poly[, 1]
                lats <- poly[, 2]
                if (length(lngs) > 0 && !all(is.na(lngs))) {
                  m <- m %>%
                    addPolygons(
                      lng = lngs, lat = lats,
                      fillColor = "#0066CC", color = "#0066CC",
                      weight = 2, opacity = 0.8, fillOpacity = 0.15,
                      popup = paste("<b>Area:</b>", spatial_filtered$name[i]),
                      label = spatial_filtered$name[i],
                      group = "Spatial Features"
                    )
                }
              }
            }
          }
        }, error = function(e) {
          cat("ERROR adding spatial feature:", spatial_filtered$name[i], "-", e$message, "\n")
        })
      }
    }
    cat("Spatial features added successfully!\n")
  }

  # Add inactive monitoring sites
  if (nrow(inactive_sites) > 0) {
    m <- m %>%
      addCircleMarkers(
        data        = inactive_sites,
        lng         = ~site_longitude, lat = ~site_latitude,
        radius      = 2,
        color       = "darkgrey",
        fillColor   = "lightgrey",
        fillOpacity = 0.4,
        stroke      = TRUE,
        weight      = 1,
        popup       = ~paste("<b>Site:</b>", name, "<br>",
                             "<b>Status:</b> No pools tested this period"),
        group       = "Monitoring Sites"
      )
    cat("Added", nrow(inactive_sites), "inactive sites\n")
  }

  # Add active monitoring sites
  if (nrow(active_sites) > 0) {
    m <- m %>%
      addCircleMarkers(
        data        = active_sites,
        lng         = ~site_longitude, lat = ~site_latitude,
        radius      = 3,
        color       = "white",
        fillColor   = "black",
        fillOpacity = 0.8,
        stroke      = TRUE,
        weight      = 2,
        popup       = ~paste("<b>Site:</b>", name, "<br>",
                             "<b>Status:</b> Active this period"),
        group       = "Monitoring Sites"
      )
    cat("Added", nrow(active_sites), "active sites\n")
  }

  # Add negative pools
  if (nrow(negative_pools) > 0) {
    negative_summary <- negative_pools %>%
      group_by(pool_longitude, pool_latitude) %>%
      summarise(
        total_pools      = sum(pool_count),
        total_mosquitoes = sum(mosquito_count),
        species_list     = paste(unique(species_display_name), collapse = ", "),
        disease_list     = paste(unique(test_target_acronym), collapse = ", "),
        county           = first(county),
        .groups          = "drop"
      )

    m <- m %>%
      addCircleMarkers(
        data        = negative_summary,
        lng         = ~pool_longitude,
        lat         = ~pool_latitude,
        radius      = ~sqrt(total_pools),
        color       = "white",
        fillColor   = "#4575b4",
        fillOpacity = 0.7,
        stroke      = TRUE,
        weight      = 1.5,
        popup       = ~paste("<b>Test Result:</b> Negative<br>",
                             "<b>Pools Tested:</b>", total_pools, "<br>",
                             "<b>Mosquitoes:</b>", total_mosquitoes, "<br>",
                             "<b>Species:</b>", species_list, "<br>",
                             "<b>Diseases Tested:</b>", disease_list),
        group       = "Negative Pools"
      )
    cat("Added", nrow(negative_summary), "negative pool locations\n")
  }

  # Add positive pools
  if (nrow(positive_pools) > 0) {
    location_summary <- positive_pools %>%
      group_by(pool_longitude, pool_latitude) %>%
      summarise(
        n_species        = n_distinct(species_display_name),
        total_pools      = sum(pool_count),
        total_mosquitoes = sum(mosquito_count),
        .groups          = "drop"
      )

    single_species_locs <- location_summary %>% filter(n_species == 1)
    multi_species_locs  <- location_summary %>% filter(n_species > 1)

    all_species    <- unique(positive_pools$species_display_name)
    species_colors <- setNames(viridis::plasma(length(all_species)), all_species)

    # Single-species positive markers
    if (nrow(single_species_locs) > 0) {
      single_data    <- positive_pools %>%
        semi_join(single_species_locs, by = c("pool_longitude", "pool_latitude"))
      species_palette <- colorFactor(species_colors, domain = all_species)

      m <- m %>%
        addCircleMarkers(
          data        = single_data,
          lng         = ~pool_longitude,
          lat         = ~pool_latitude,
          radius      = ~sqrt(pool_count) * 4,
          color       = "#d73027",
          fillColor   = ~species_palette(species_display_name),
          fillOpacity = 0.9,
          stroke      = TRUE,
          weight      = 3,
          popup       = ~paste("<b style='color:red;'>POSITIVE</b><br>",
                               "<b>Disease:</b>", test_target_acronym, "<br>",
                               "<b>Species:</b>", species_display_name, "<br>",
                               "<b>Pools:</b>", pool_count, "<br>",
                               "<b>Mosquitoes:</b>", mosquito_count),
          group       = "Positive Pools"
        )
      cat("Added", nrow(single_data), "single-species positive markers\n")
    }

    # Multi-species positive pie charts
    if (nrow(multi_species_locs) > 0) {
      pie_wide <- positive_pools %>%
        semi_join(multi_species_locs, by = c("pool_longitude", "pool_latitude")) %>%
        group_by(pool_longitude, pool_latitude, species_display_name) %>%
        summarise(species_pools = sum(pool_count), .groups = "drop") %>%
        group_by(pool_longitude, pool_latitude) %>%
        mutate(total_pools = sum(species_pools)) %>%
        ungroup() %>%
        pivot_wider(
          id_cols      = c(pool_longitude, pool_latitude, total_pools),
          names_from   = species_display_name,
          values_from  = species_pools,
          values_fill  = 0
        )

      if (nrow(pie_wide) > 0) {
        species_cols <- intersect(all_species, names(pie_wide))

        if (length(species_cols) >= 2) {
          chartdata  <- as.matrix(pie_wide[, species_cols])
          if (!is.numeric(chartdata)) chartdata <- apply(chartdata, 2, as.numeric)
          pie_colors <- unname(species_colors[species_cols])

          m <- m %>%
            addMinicharts(
              lng            = pie_wide$pool_longitude,
              lat            = pie_wide$pool_latitude,
              type           = "pie",
              chartdata      = chartdata,
              colorPalette   = pie_colors,
              width          = 50 * sqrt(pie_wide$total_pools / max(pie_wide$total_pools)),
              transitionTime = 0,
              opacity        = 0.95
            )
          cat("Added", nrow(pie_wide), "multi-species positive pie chart markers\n")
        }
      }
    }

    # Species legend for positive pools
    m <- m %>%
      addLegend(
        position = "bottomright",
        colors   = species_colors,
        labels   = names(species_colors),
        title    = "Species (Positive Pools)",
        opacity  = 1
      )
  }

  cat("Added test result legend\n")

  # Test result legend
  m <- m %>%
    addLegend(
      position = "topright",
      colors   = c("#d73027", "#4575b4"),
      labels   = c("Positive", "Negative"),
      title    = "Test Results",
      opacity  = 1
    )

  # Build title
  title_text <- paste0(
    "<div style='background:white;padding:8px;border-radius:4px;'>",
    "<b>Mosquito Pool Testing - ",
    if (!is.null(time_period) && interval != "Year") paste0(time_period, " ") else "",
    target_year, "</b>",
    if (!is.null(target_disease)) paste0("<br><i>", paste(target_disease, collapse = ", "), "</i>") else "",
    if (!is.null(spatial_features)) paste0("<br>Areas: ", paste(spatial_features, collapse = ", ")) else "",
    "</div>"
  )

  m <- m %>%
    addControl(html = title_text, position = "topleft") %>%
    addScaleBar(position = "bottomleft")

  # Add layers control (HTML only)
  groups <- c()
  if (!is.null(agency) && nrow(agency) > 0)           groups <- c(groups, "Agency Boundaries")
  if (!is.null(spatial_filtered) && nrow(spatial_filtered) > 0) groups <- c(groups, "Spatial Features")
  groups <- c(groups, "Monitoring Sites", "Negative Pools", "Positive Pools")

  if (html) {
    m <- m %>%
      addLayersControl(
        overlayGroups = groups,
        options       = layersControlOptions(collapsed = FALSE)
      )
  }

  # Set map bounds
  if (!is.null(spatial_filtered) && nrow(spatial_filtered) > 0) {
    all_lngs <- c()
    all_lats <- c()

    for (i in 1:nrow(spatial_filtered)) {
      coords <- spatial_filtered$shape.coordinates[[i]]
      if (!is.null(coords) && length(coords) > 0) {
        if (is.array(coords) && !is.list(coords)) {
          coords_dim <- dim(coords)
          for (poly_idx in 1:coords_dim[1]) {
            for (ring_idx in 1:coords_dim[2]) {
              coords_matrix <- coords[poly_idx, ring_idx, , ]
              if (is.matrix(coords_matrix)) {
                all_lngs <- c(all_lngs, coords_matrix[, 1])
                all_lats <- c(all_lats, coords_matrix[, 2])
              }
            }
          }
        } else if (is.list(coords)) {
          for (poly_idx in 1:length(coords)) {
            poly <- coords[[poly_idx]]
            if (is.array(poly)) {
              poly_dim <- dim(poly)
              if (length(poly_dim) == 3 && poly_dim[1] == 1 && poly_dim[3] == 2) {
                coords_matrix <- poly[1, , ]
                if (is.matrix(coords_matrix)) {
                  all_lngs <- c(all_lngs, coords_matrix[, 1])
                  all_lats <- c(all_lats, coords_matrix[, 2])
                }
              }
            } else if (is.matrix(poly)) {
              all_lngs <- c(all_lngs, poly[, 1])
              all_lats <- c(all_lats, poly[, 2])
            }
          }
        }
      }
    }

    if (length(all_lngs) > 0) {
      m <- m %>%
        fitBounds(min(all_lngs, na.rm = TRUE), min(all_lats, na.rm = TRUE),
                  max(all_lngs, na.rm = TRUE), max(all_lats, na.rm = TRUE))
    }
  } else if (nrow(data) > 0) {
    m <- m %>%
      fitBounds(
        min(data$pool_longitude, na.rm = TRUE),
        min(data$pool_latitude,  na.rm = TRUE),
        max(data$pool_longitude, na.rm = TRUE),
        max(data$pool_latitude,  na.rm = TRUE)
      )
  }

  if (html) {
    suppressWarnings(return(m))
  } else {
    return(.show_map(m, height = height))
  }
}

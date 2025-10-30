#' Plot Vector Abundance Over Time with Comparison to Historical Averages
#'
#' This function creates a time series plot showing vector (mosquito) abundance over multiple years,
#' highlighting a target year in red and optionally showing a historical average line. The plot
#' helps visualize trends and anomalies in vector populations.
#'
#' @param token Authentication token for API access, typically obtained from `getToken()`
#' @param interval Time interval for aggregation. One of: "CollectionDate", "Week", "Biweek", "Month"
#' @param target_year The focal year to highlight in the plot
#' @param start_year Starting year for data retrieval. If NULL, defaults to 5 years before target_year
#' @param end_year Ending year for data retrieval (integer). If NULL, defaults to target_year
#' @param species Character vector for filtering species. View species in your data `unique(data$species_display_name)`. Defaults to all species if no selection
#' @param trap Character vector for filtering trap type by acronym. View trap types in your data`unique(data$trap_acronym`. Defaults to all trap types
#' @param sex Character vector for filtering sex type. View sex options `unique(data$sex_type`). Defaults to "female".
#' @param agency_ids Agency identifier to filter data if applicable
#' @param trapnight_min Minimum number of trap nights to include (numeric). Defaults to 1
#' @param trapnight_max Maximum number of trap nights to include (numeric). If NULL, uses maximum available
#' @importFrom scales hue_pal

#' @return A ggplot object showing vector abundance over time, with the target year highlighted in red
#' and historical average shown in blue
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' token <- getToken()
#' plot <- plotVectorAbundance(
#'   token = token,
#'   interval = "Week",
#'   target_year = 2024,
#'   species = "Cx pipiens",
#'   trap = "CO2"
#' )
#'
#' # Compare multiple years with custom date range
#' plot <- plotVectorAbundance(
#'   token = token,
#'   interval = "Month",
#'   target_year = 2024,
#'   start_year = 2020,
#'   end_year = 2024,
#'   species = "Ae aegypti",
#'   trap = c("BG", "CDC"),
#'   sex = "female",
#'   trapnight_min = 3
#' )
#' }
#'
#' @importFrom dplyr %>%
#' @import ggplot2
#' @importFrom scales hue_pal
#' @export
plotVectorAbundance <- function(token, interval, target_year, start_year = NULL, end_year = NULL, species, trap, agency_ids = NULL, sex = "female", trapnight_min = 1, trapnight_max = NULL) {

  # Validate input parameters
  if (is.null(end_year)) {
    end_year <- target_year
  }

  if (!is.null(start_year)) {
    if (start_year >= target_year) {
      stop("Target year cannot be greater or equal to start year")
    }
  } else {
    start_year <- target_year - 5
  }

  # Retrieve arthropod collection data
  data <- getArthroCollections(token, start_year = start_year, end_year = end_year, 'mosquito', agency_ids = agency_ids)

  # Validate species species
  if (length(species) > 1) {
    stop("More than one species detected, select only one species as a vector for this graph")
  }

  if (!(target_year %in% unique(data$surv_year))) {
    stop("Target year not in data. Please submit data with target year")
  }

  if (!(species %in% unique(data$species_display_name))) {
    invalid_species <- setdiff(species, unique(data$species_display_name))
    valid_species <- unique(data$species_display_name)

    stop(paste("Invalid species input:", paste(invalid_species, collapse = ", "),
               "\nPlease select one of the following species:", paste(valid_species, collapse = ", ")))
  }

  # Validate trap types
  if (!all(trap %in% unique(data$trap_acronym))) {
    invalid_traps <- setdiff(trap, unique(data$trap_acronym))
    valid_traps <- unique(data$trap_acronym)

    stop(paste("Invalid trap(s):", paste(invalid_traps, collapse = ", "),
               "\nAvailable traps:", paste(valid_traps, collapse = ", ")))
  }

  # Calculate abundance data
  abundance_data <- getAbundance(data, interval, agency = unique(data$agency_code),
                                 species = species, trap = trap, sex = sex,
                                 trapnight_min = trapnight_min, trapnight_max = trapnight_max)

  # Calculate historical averages
  average_data <- getAbundanceAnomaly(data, interval, target_year, species = species, trap = trap, sex = sex,
                                      trapnight_min = trapnight_min, trapnight_max = trapnight_max)

  # Convert interval columns to factors with proper ordering
  if (interval == "CollectionDate") {
    # Extract just the month-day part and format as character
    abundance_data[[interval]] <- format(as.Date(abundance_data[[interval]]), "%m-%d")

    # Order the dates chronologically for proper plotting
    date_levels <- unique(abundance_data[[interval]])
    date_levels <- date_levels[order(as.Date(paste0("2000-", date_levels)))]
    abundance_data[[interval]] <- factor(abundance_data[[interval]], levels = date_levels)

    # Also format average_data if it exists
    if (!is.null(average_data) && nrow(average_data) > 0) {
      average_data[[interval]] <- format(as.Date(average_data[[interval]]), "%m-%d")
      average_data[[interval]] <- factor(average_data[[interval]], levels = date_levels)
    }

  } else {
    # For other intervals, convert to factor with natural ordering
    if (is.numeric(abundance_data[[interval]])) {
      # If numeric (like Week), sort numerically
      abundance_data[[interval]] <- factor(abundance_data[[interval]],
                                           levels = sort(unique(abundance_data[[interval]])))
    } else {
      # If character (like Month), sort alphabetically or use natural ordering
      abundance_data[[interval]] <- factor(abundance_data[[interval]],
                                           levels = sort(unique(abundance_data[[interval]])))
    }

    # Also convert average_data if it exists
    if (!is.null(average_data) && nrow(average_data) > 0) {
      if (is.numeric(average_data[[interval]])) {
        average_data[[interval]] <- factor(average_data[[interval]],
                                           levels = sort(unique(average_data[[interval]])))
      } else {
        average_data[[interval]] <- factor(average_data[[interval]],
                                           levels = sort(unique(average_data[[interval]])))
      }
    }
  }

  # Create a custom color scale
  all_years <- sort(unique(abundance_data$Year))
  color_values <- ifelse(all_years == target_year, "red3",
                         scales::hue_pal()(length(all_years)))
  names(color_values) <- all_years

  # Add mean to the color scale
  color_values <- c(color_values, "Mean" = "blue4")

  # Calculate appropriate breaks for x-axis
  if (interval == "CollectionDate") {
    # For dates, sample at most 52 evenly spaced breaks
    all_intervals <- levels(abundance_data[[interval]])
    n_breaks <- min(52, length(all_intervals))

    break_indices <- seq(1, length(all_intervals), length.out = n_breaks) %>%
      as.integer()

    x_breaks <- all_intervals[break_indices]

  } else {
    # For other intervals, use all levels (they're already factors)
    x_breaks <- levels(abundance_data[[interval]])
  }

  # Names for traps
  trap_list <- paste(trap, collapse = ", ")

  # Create the base plot
  p <- ggplot(data = abundance_data, aes(x = .data[[interval]], y = Abundance,
                                         color = factor(Year), group = factor(Year))) +
    geom_line(aes(linewidth = ifelse(Year == target_year, "target", "other"))) +
    geom_point(aes(size = ifelse(Year == target_year, "target", "other"))) +
    scale_linewidth_manual(name = NULL, values = c("other" = 0.5, "target" = 1),
                           guide = "none") +
    scale_size_manual(name = NULL, values = c("other" = 1, "target" = 2),
                      guide = "none") +
    scale_x_discrete(breaks = x_breaks) +
    labs(title = paste("Vector Abundance"),
         subtitle = paste(species, "in", trap_list, "traps"),
         caption = paste("Target year", target_year, "highlighted in red"),
         x = interval,
         y = "Abundance") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = ifelse(interval == "CollectionDate", 45, 0),
                                     hjust = ifelse(interval == "CollectionDate", 1, 0.5)))

  # Add mean line if average_data exists and has data
  if (!is.null(average_data) && nrow(average_data) > 0) {
    p <- p +
      geom_line(data = average_data,
                aes(x = .data[[interval]], y = FiveYearAvg, group = 1, color = "Mean"),
                linewidth = 1.2) +
      geom_point(data = average_data,
                 aes(x = .data[[interval]], y = FiveYearAvg, color = "Mean"),
                 size = 2)
  }

  # Add color scale after all geoms
  p <- p + scale_color_manual(name = "Year", values = color_values)

  return(p)
}

#' Plot cumulative sum and interval sums of invasive species
#'
#' This function graphs the cumulative sum of invasive mosquito specie(s) as a line chart over a selected interval.The bar chart shows the interval sum of invasive specie(s).
#' For cumulative sum, a convex slope indicates increased rate of species trappings, concave a decrease in rate, and liner a constant rate.
#'
#' @param token A valid access token returned from `getToken()`
#' @param interval Calculation interval for abundance, accepts "CollectionDate",“Week”, “Biweek”, or “Month.
#' @param target_year Year to plot
#' @param invasive_species Names of invasive species to be plotted, multiple invasive species are entered in vector format i.e. `invasive_species = c("Ae aegypti", "Ae albopictus")`
#' @param agency_ids An optional vector for filtering agency by id

plotInvasiveSums <- function(token, interval, target_year, invasive_species, agency_ids = NULL) {
  # Get data for the last 5 years plus current year
  data <- getArthroCollections(token, start_year = target_year, end_year = target_year, arthropod = 'mosquito', agency_ids = agency_ids)

  if(!any(invasive_species %in% unique(data$species_display_name))) {
    invalid_species <- setdiff(invasive_species, unique(data$species_display_name))
    valid_species <- unique(data$species_display_name)

    stop(paste("Invalid species input:", paste(invalid_species, collapse = ", "),
               "\nPlease select one of the following species:", paste(valid_species, collapse = ", ")))
  }




  # Get abundance data
  ab <- getAbundance(data, interval = interval, species = invasive_species, separate_by = "species")

  # Calculate cumulative sum by species and interval group
  ab <- ab %>%
    arrange(Species, .data[[interval]]) %>%
    group_by(Species) %>%
    mutate(cumsum = cumsum(Count)) %>%
    ungroup()

  # Convert ALL interval columns to factors with proper ordering
  if (interval == "CollectionDate") {
    # First, ensure the CollectionDate is in proper Date format
    ab[[interval]] <- as.Date(ab[[interval]])

    # Extract just the month-day part and format as character
    ab[[interval]] <- format(ab[[interval]], "%m-%d")

    # Order the dates chronologically for proper plotting
    date_levels <- unique(ab[[interval]])

    # Safe date ordering with error handling
    date_levels <- tryCatch({
      # Create temporary dates with a common year for proper ordering
      temp_dates <- as.Date(paste0("2000-", date_levels), format = "%Y-%m-%d")
      date_levels[order(temp_dates)]
    }, error = function(e) {
      # If date parsing fails, fall back to alphabetical order
      warning("Unable to parse dates chronologically, using alphabetical order instead")
      sort(date_levels)
    })

    ab[[interval]] <- factor(ab[[interval]], levels = date_levels)

  } else {
    # For other intervals, convert to factor with natural ordering
    if (is.numeric(ab[[interval]])) {
      # If numeric (like Week), sort numerically
      ab[[interval]] <- factor(ab[[interval]],
                               levels = sort(unique(ab[[interval]])))
    } else {
      # If character (like Month), sort alphabetically or use natural ordering
      ab[[interval]] <- factor(ab[[interval]],
                               levels = sort(unique(ab[[interval]])))
    }
  }

  # Calculate appropriate breaks for x-axis
  if (interval == "CollectionDate") {
    # For dates, sample at most 52 evenly spaced breaks
    all_intervals <- levels(ab[[interval]])
    n_breaks <- min(52, length(all_intervals))

    break_indices <- seq(1, length(all_intervals), length.out = n_breaks) %>%
      as.integer()

    x_breaks <- all_intervals[break_indices]

  } else {
    # For other intervals, use all levels (they're already factors)
    x_breaks <- levels(ab[[interval]])
  }

  # Create the plot
  p <- ggplot(ab, aes(x = .data[[interval]])) +
    geom_col(aes(y = Count, fill = Species), position = "dodge", alpha = 0.7) +
    geom_line(aes(y = cumsum, color = Species, group = Species),
              linewidth = 1, alpha = 0.8) +
    geom_point(aes(y = cumsum, color = Species, group = Species), size = 2) +
    labs(
      title = paste("Invasive Species:", paste(invasive_species, collapse = ", ")),
      subtitle = paste("Counts and Cumulative Sum -", year),
      x = interval,
      y = "Count",
      fill = "Species",
      color = "Species"
    ) +
    scale_x_discrete(breaks = x_breaks) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = ifelse(interval == "CollectionDate", 90, 0),
                                     hjust = ifelse(interval == "CollectionDate", 1, 0.5),
                                     vjust = ifelse(interval == "CollectionDate", 0.5, 0))) +
    scale_y_continuous(
      sec.axis = sec_axis(~., name = "Cumulative Sum")
    )

  return(p)
}

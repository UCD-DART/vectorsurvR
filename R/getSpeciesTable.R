#' Get Table For Species
#'
#' Allows users to create a custom table for vector species. The invasive
#' species count, trap nights, and abundance are calculated by chosen groupings
#' or time intervals. The table has output options based on document format.
#'
#' @param token Authentication token for API access, obtained from `getToken()`
#' @param interval Time interval for aggregation. One of: "CollectionDate", "Week", "Biweek", "Month", or NULL for year-level summary
#' @param target_year The focal year to highlight in the plot (integer)
#' @param cumulative T/F Adds columns for the cumulative sum count Default to FALSE
#' @param include_abundance T/F Adds column for abundance (non-cumulative only)
#' @param species Character vector for filtering species. View species in your data `unique(data$species_display_name)`. Defaults to all species if no selection
#' @param trap Character vector for filtering trap type by acronym. View trap types in your data`unique(data$trap_acronym`. Defaults to all trap types
#' @param sex Character vector for filtering sex type. View sex options `unique(data$sex_type`). Defaults to "female".
#' @param agency_id Agency identifier to filter data if applicable
#' @param separate_by Adds a column from the data as a grouping variable.
#' Accepts 'site', 'city', 'county', 'agency', 'trap', 'spatial'
#' @param output_format Format to output table display. Accepts 'html', 'pdf',
#'  'word' or 'auto'. Default auto returns the console format dataframe.
#' @param caption Caption for table
#' @import kableExtra
#' @return A table displaying invasive species interval or/and cumulative counts over time
#' @export

getSpeciesTable <- function(token,
                            interval = NULL,
                            target_year,
                            cumulative = FALSE,
                            include_abundance = FALSE,
                            species = NULL,
                            trap = NULL,
                            sex = "female",
                            separate_by = NULL,
                            output_format = "auto",
                            caption = NULL,
                            agency_id = NULL) {

  # Validate separate_by parameter
  if (!is.null(separate_by)) {
    valid_groupings <- c("site", "city", "county", "agency", "trap", "spatial")
    invalid_groupings <- setdiff(separate_by, valid_groupings)

    if (length(invalid_groupings) > 0) {
      stop(
        "Invalid grouping(s): ", paste(invalid_groupings, collapse = ", "), "\n",
        "Valid options are: ", paste(valid_groupings, collapse = ", ")
      )
    }
  }

  # Get collections data
  if(any(c("city","county","spatial") %in% separate_by)){
    collections <- getArthroCollections(token, target_year, target_year,
                                        arthropod = 'mosquito', agency_id, geocoded = T)
  } else {
    collections <- getArthroCollections(token, target_year, target_year,
                                        arthropod = 'mosquito', agency_id, geocoded = F)
  }

  # Get abundance data
  # If interval is NULL, use "Year" as a placeholder that we'll handle
  if (is.null(interval)) {
    abundance_data <- getAbundance(
      collections = collections,
      interval = "CollectionDate",  # Get the most granular data
      species = species,
      trap = trap,
      sex = sex,
      separate_by = separate_by
    )

    # Aggregate to year level by removing the interval column and summarizing
    group_vars <- c("Species")
    if (!is.null(separate_by)) {
      input_to_display <- c(
        "site" = "Site_Name",
        "city" = "City",
        "county" = "County",
        "agency" = "Agency",
        "trap" = "Trap",
        "spatial" = "Spatial"
      )
      display_names_temp <- input_to_display[separate_by]
      display_names_temp <- display_names_temp[!is.na(display_names_temp)]
      group_vars <- c(group_vars, display_names_temp)
    }

    # Aggregate to year level - no abundance column since it doesn't make sense for yearly data
    abundance_data <- abundance_data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        Count = sum(Count, na.rm = TRUE),
        TrapEvents = sum(TrapEvents, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    abundance_data <- getAbundance(
      collections = collections,
      interval = interval,
      species = species,
      trap = trap,
      sex = sex,
      separate_by = separate_by
    )
  }

  # Add Year column
  abundance_data <- abundance_data %>%
    mutate(Year = target_year)

  # Keep the nice display names from getAbundance output
  # Map from user input (short names) to getAbundance column names (formatted display names)
  if (!is.null(separate_by)) {
    input_to_display <- c(
      "site" = "Site_Name",
      "city" = "City",
      "county" = "County",
      "agency" = "Agency",
      "trap" = "Trap",
      "spatial" = "Spatial"
    )

    # Replace separate_by with the actual column names that exist in the data
    separate_by <- input_to_display[separate_by]
    separate_by <- separate_by[!is.na(separate_by)]
  }

  # Calculate cumulative values if requested
  if (cumulative) {
    cum_group_vars <- c("Species")
    if (!is.null(separate_by)) {
      cum_group_vars <- c(cum_group_vars, separate_by)
    }

    # Determine ordering variables
    order_vars <- cum_group_vars
    if (!is.null(interval)) {
      order_vars <- c(order_vars, interval)
    }

    abundance_data <- abundance_data %>%
      arrange(across(all_of(order_vars))) %>%
      group_by(across(all_of(cum_group_vars))) %>%
      mutate(
        Cumulative_Count = cumsum(Count),
        Cumulative_TrapEvents = cumsum(TrapEvents)
      ) %>%
      ungroup()
  }

  # Build column selection dynamically
  display_cols <- c()
  display_names <- c()

  # Add interval if provided
  if (!is.null(interval)) {
    display_cols <- c(display_cols, interval)
    display_names <- c(display_names, interval)
  }

  # Add Year column after interval
  display_cols <- c(display_cols, "Year")
  display_names <- c(display_names, "Year")

  # Add Species
  display_cols <- c(display_cols, "Species")
  display_names <- c(display_names, "Species")

  # Add grouping columns
  if (!is.null(separate_by)) {
    display_cols <- c(display_cols, separate_by)
    display_names <- c(display_names, separate_by)
  }

  # Add count and trap event columns based on cumulative flag
  if (cumulative) {
    display_cols <- c(display_cols, "Cumulative_Count", "Cumulative_TrapEvents")
    display_names <- c(display_names, "Cumulative Count", "Cumulative Trap Events")
  } else {
    display_cols <- c(display_cols, "Count", "TrapEvents")
    display_names <- c(display_names, "Count", "Trap Events")

    # Only add abundance for non-cumulative and when interval is not NULL
    if (include_abundance && !is.null(interval) && "Abundance" %in% names(abundance_data)) {
      display_cols <- c(display_cols, "Abundance")
      display_names <- c(display_names, "Abundance")
    }
  }

  # Select only columns that exist in the data
  existing_cols <- intersect(display_cols, names(abundance_data))
  display_data <- abundance_data %>% select(all_of(existing_cols))

  # Filter display_names to match existing columns
  display_names <- display_names[display_cols %in% existing_cols]

  # Generate caption if not provided
  if (is.null(caption)) {
    grouping_text <- if (!is.null(separate_by)) {
      paste("Grouped by:", paste(separate_by, collapse = ", "))
    } else {
      "Overall"
    }

    interval_text <- if (!is.null(interval)) {
      paste("- Interval:", interval)
    } else {
      "- Year Summary"
    }

    caption <- paste(
      "Mosquito Collection Summary -", target_year,
      ifelse(cumulative, "(Cumulative)", "(Non-cumulative)"),
      interval_text, "-", grouping_text
    )
  }

  # Determine output format
  if (output_format == "auto") {
    if (knitr::is_latex_output()) {
      output_format <- "latex"
    } else if (knitr::is_html_output()) {
      output_format <- "html"
    } else {
      output_format <- "simple"
    }
  }

  # Return based on output format
  if (output_format == "simple") {
    return(display_data)
  }

  # Calculate number of label vs numeric columns for alignment
  label_col_names <- c(interval, "Year", "Species", separate_by)
  label_col_names <- label_col_names[!is.na(label_col_names) & !is.null(label_col_names)]
  n_label_cols <- length(label_col_names)
  n_numeric_cols <- length(display_names) - n_label_cols

  # Create kable table
  if (output_format == "word") {
    kable_table <- display_data %>%
      kableExtra::kable(
        format = "pipe",
        caption = caption,
        col.names = display_names
      )
    return(kable_table)
  }

  # HTML or LaTeX output
  kable_table <- display_data %>%
    kableExtra::kable(
      format = ifelse(output_format == "html", "html", "latex"),
      caption = caption,
      align = c(rep('l', n_label_cols), rep('r', n_numeric_cols)),
      col.names = display_names,
      booktabs = TRUE
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = if (output_format == "html") c("striped", "hover", "condensed"),
      latex_options = if (output_format %in% c("pdf", "latex")) c("striped", "hold_position"),
      full_width = FALSE
    )

  if (output_format == "html") {
    kable_table <- kable_table %>%
      kableExtra::row_spec(0, bold = TRUE, background = "#2C3E50", color = "white")
  }

  return(kable_table)
}

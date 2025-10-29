#' @title Calculate abundance
#'
#' @description Calculates abundance with option for filtering data and grouping
#' results
#' @param  collections Collections data retrieved from getArthroCollections()
#' @param interval Calculation interval for abundance, accepts "CollectionDate","Week", "Biweek", or "Month.
#' @param agency An optional vector for filtering agency by character code
#' @param species Character vector for filtering species. View species in your data `unique(data$species_display_name)`. Defaults to all species if no selection
#' @param trap Character vector for filtering trap type by acronym. View trap types in your data`unique(data$trap_acronym`. Defaults to all trap types
#' @param sex Character vector for filtering sex type. View sex options `unique(data$sex_type`). Defaults to "female".
#' @param trapnight_min Minimum trap night restriction for calculation. Default is 1.
#' @param trapnight_max Maximum trap night restriction for calculation. Default is no restriction.
#' @param separate_by Separate/group the calculation by 'trap','species', 'agency', 'county', or 'spatial'. Default NULL does not separate.
#' @return A dataframe of abundance calculations.
#' @export
#' @importFrom dplyr across all_of full_join left_join distinct ungroup
#' @importFrom tidyr pivot_wider pivot_longer any_of crossing
#' @importFrom lubridate year month epiweek today
#' @importFrom utils tail
#' @examples
#' getAbundance(sample_collections,
#'              interval = 'Week',
#'              species = list('Cx pipiens'),
#'              trap = list('GRVD', 'CO2'),
#'              sex = list("female"),
#'              trapnight_min = 1,
#'              trapnight_max = 5,
#'              separate_by  = "species")

getAbundance <- function(collections, interval, agency = NULL, species = NULL, trap = NULL, sex = "female", trapnight_min = 1, trapnight_max=NULL, separate_by = NULL) {

  if (nrow(collections) <= 0) {
    stop("Collections data is empty")
  }

  required_columns <- c("collection_id", "collection_date", "agency_code","num_trap", "trap_nights",
                        "trap_problem_bit", "num_count", "sex_type", "species_display_name",
                        "trap_acronym")
  separate_options <- c("agency","site", "city","species", "trap", "spatial", "county")

  if (any(!(required_columns %in% colnames(collections)))) {
    stop("Insufficient collections data provided")
  }
  if(!is.null(separate_by) && any(!separate_by %in% separate_options)){
    stop("Check separate_by parameters. Accepted options are 'species', 'trap','county', 'agency', or 'spatial'")
  }

  collections <- collections %>%
    dplyr::filter(trap_nights != 0, num_trap != 0, trap_problem_bit == FALSE)

  if (!interval %in% c("CollectionDate","Week", "Biweek", "Month")) {
    stop("Incorrect interval input. Interval accepts inputs of 'CollectionDate','Week', 'Biweek', or 'Month'")
  }

  if("spatial" %in% separate_by){
    if(!"spatial_feature"%in%colnames(collections)){
      stop("No spatial_features found in collections data")
    }
    if("TRUE" %in% unique(collections$multiple_features)){
      stop("There is overlapping spatial features in data. Please ensure no spatial features overlap when seperating calculation by spatial. Overlapping spatial features can lead to skewed abundance results.")
    }
  }

  if(is.null(trapnight_max)){
    trapnight_max = max(collections$trap_nights)
  }
  if(trapnight_min<1){
    stop("Invalid minimum trap night entered")
  }

  collections$INTERVAL <- switch(interval,
                                 "CollectionDate" = as.Date(collections$collection_date),
                                 "Week" = as.numeric(epiweek(collections$collection_date)),
                                 "Biweek" = as.numeric(ceiling(epiweek(collections$collection_date) / 2)),
                                 "Month" = as.numeric(month(collections$collection_date)))

  if (is.null(agency)) {
    agency <- unique(collections$agency_code)
  }
  if (is.null(species)) {
    species <- unique(collections$species_display_name)
  }
  if (is.null(trap)) {
    trap <- unique(collections$trap_acronym)
  }
  if (is.null(sex)) {
    sex <- unique(collections$sex_type)
  }

  # Handle dynamic grouping - SEPARATE FOR COUNTS VS TRAP EVENTS
  base_grouping_vars <- c("surv_year", "INTERVAL")

  # For counts: include species if specified in separate_by
  grouping_vars_counts <- base_grouping_vars
  # For trap events: NEVER include species
  grouping_vars_trap <- base_grouping_vars

  if (!is.null(separate_by)) {
    if ("species" %in% separate_by) {
      grouping_vars_counts <- c(grouping_vars_counts, "species_display_name")
      # Note: species NOT added to trap events grouping
    }
    if ("agency" %in% separate_by) {
      grouping_vars_counts <- c(grouping_vars_counts, "agency_code")
      grouping_vars_trap <- c(grouping_vars_trap, "agency_code")
    }
    if ("spatial" %in% separate_by) {
      grouping_vars_counts <- c(grouping_vars_counts, "spatial_feature")
      grouping_vars_trap <- c(grouping_vars_trap, "spatial_feature")
    }
    if ("trap" %in% separate_by) {
      grouping_vars_counts <- c(grouping_vars_counts, "trap_acronym")
      grouping_vars_trap <- c(grouping_vars_trap, "trap_acronym")
    }
    if ("county" %in% separate_by) {
      grouping_vars_counts <- c(grouping_vars_counts, "county")
      grouping_vars_trap <- c(grouping_vars_trap, "county")
    }
    if ("city" %in% separate_by) {
      grouping_vars_counts <- c(grouping_vars_counts, "city")
      grouping_vars_trap <- c(grouping_vars_trap, "city")
    }
    if ("site" %in% separate_by) {
      grouping_vars_counts <- c(grouping_vars_counts, "site_name")
      grouping_vars_trap <- c(grouping_vars_trap, "site_name")
    }
  }

  # Calculate Counts - with species grouping if needed
  cts <- collections %>%
    dplyr::filter(agency_code %in% agency,
                  species_display_name %in% species,
                  trap_acronym %in% trap,
                  sex_type %in% sex,
                  trap_nights>= trapnight_min,
                  trap_nights<=trapnight_max) %>%
    dplyr::group_by(across(all_of(grouping_vars_counts))) %>%
    dplyr::summarise(
      Count = sum(num_count, na.rm = TRUE),
      Species = paste(sort(unique(species_display_name)), collapse = ", "),
      Agency = paste(sort(unique(agency_code)), collapse = ", "),
      County = paste(sort(unique(county)), collapse = ", "),
      .groups = "drop"
    ) %>%
    as.data.frame()

  # ADDED: Include city and site_name columns if they were used in grouping
  if ("city" %in% separate_by && "city" %in% grouping_vars_counts) {
    cts_city <- collections %>%
      dplyr::filter(agency_code %in% agency,
                    species_display_name %in% species,
                    trap_acronym %in% trap,
                    sex_type %in% sex,
                    trap_nights>= trapnight_min,
                    trap_nights<=trapnight_max) %>%
      dplyr::group_by(across(all_of(grouping_vars_counts))) %>%
      dplyr::summarise(
        City = paste(sort(unique(city)), collapse = ", "),
        .groups = "drop"
      ) %>%
      as.data.frame()

    cts <- merge(cts, cts_city, by = grouping_vars_counts, all.x = TRUE)
  }

  if ("site" %in% separate_by && "site_name" %in% grouping_vars_counts) {
    cts_site <- collections %>%
      dplyr::filter(agency_code %in% agency,
                    species_display_name %in% species,
                    trap_acronym %in% trap,
                    sex_type %in% sex,
                    trap_nights>= trapnight_min,
                    trap_nights<=trapnight_max) %>%
      dplyr::group_by(across(all_of(grouping_vars_counts))) %>%
      dplyr::summarise(
        Site_Name = paste(sort(unique(site_name)), collapse = ", "),
        .groups = "drop"
      ) %>%
      as.data.frame()

    cts <- merge(cts, cts_site, by = grouping_vars_counts, all.x = TRUE)
  }

  # Add spatial column if needed
  if ("spatial" %in% separate_by) {
    cts_spatial <- collections %>%
      dplyr::filter(agency_code %in% agency,
                    species_display_name %in% species,
                    trap_acronym %in% trap,
                    sex_type %in% sex,
                    trap_nights>= trapnight_min,
                    trap_nights<=trapnight_max) %>%
      dplyr::group_by(across(all_of(grouping_vars_counts))) %>%
      dplyr::summarise(
        Spatial = paste(sort(unique(spatial_feature)), collapse = ", "),
        .groups = "drop"
      ) %>%
      as.data.frame()

    cts <- merge(cts, cts_spatial, by = grouping_vars_counts, all.x = TRUE)
  }

  # Calculate Trap Events - WITHOUT species grouping
  tns <- collections %>%
    dplyr::filter(agency_code %in% agency,
                  trap_acronym %in% trap) %>%
    distinct(collection_id, .keep_all = TRUE) %>%
    dplyr::group_by(across(all_of(grouping_vars_trap))) %>%
    dplyr::summarise(
      TrapEvents = sum(trap_nights * num_trap, na.rm = TRUE),
      Trap = paste(sort(unique(trap_acronym)), collapse = ", "),
      .groups = "drop"
    ) %>%
    as.data.frame()

  # Create a complete set of all possible combinations for the base grouping
  unique_combinations <- collections %>%
    dplyr::filter(agency_code %in% agency, trap_acronym %in% trap) %>%
    dplyr::select(all_of(grouping_vars_trap)) %>%
    dplyr::distinct()

  # Create all combinations using expand.grid
  combo_list <- list()
  for (var in grouping_vars_trap) {
    combo_list[[var]] <- unique(unique_combinations[[var]])
  }

  all_combinations <- expand.grid(combo_list, stringsAsFactors = FALSE)

  # If species is in separate_by, we need to handle it differently
  if ("species" %in% separate_by) {
    # First, merge trap events with base combinations (without species)
    tns_complete_base <- full_join(all_combinations, tns, by = grouping_vars_trap)

    # Get unique values for each grouping variable from tns_complete_base
    base_unique <- list()
    for (var in grouping_vars_trap) {
      base_unique[[var]] <- unique(tns_complete_base[[var]])
    }

    # Add species to the list - ensure it's a character vector
    base_unique[["species_display_name"]] <- as.character(species)

    # Create all combinations including species
    all_combos_with_species <- expand.grid(base_unique, stringsAsFactors = FALSE)

    # Ensure species_display_name is character type (not list)
    all_combos_with_species$species_display_name <- as.character(all_combos_with_species$species_display_name)

    # Merge with trap events (which doesn't have species_display_name)
    tns_complete <- left_join(all_combos_with_species,
                              tns_complete_base,
                              by = grouping_vars_trap)

    # Set merge_vars to include species_display_name
    merge_vars <- c(grouping_vars_trap, "species_display_name")

  } else {
    # No species separation - simple merge
    tns_complete <- full_join(all_combinations, tns, by = grouping_vars_trap)
    merge_vars <- grouping_vars_trap
  }

  # Now merge with counts using the correct merge_vars
  AB <- left_join(tns_complete, cts, by = merge_vars)

  # Handle NA values after merge - replace NA counts with 0 (trap was set but nothing caught)
  AB$Count[is.na(AB$Count)] <- 0

  # Handle Species column
  if ("species_display_name" %in% colnames(AB)) {
    AB$Species[is.na(AB$Species)] <- AB$species_display_name[is.na(AB$Species)]
  } else {
    AB$Species[is.na(AB$Species)] <- "No Collections"
  }

  # Handle Agency column
  if ("agency_code" %in% colnames(AB)) {
    AB$Agency[is.na(AB$Agency)] <- ifelse(is.na(AB$agency_code[is.na(AB$Agency)]),
                                          paste(agency, collapse = ", "),
                                          AB$agency_code[is.na(AB$Agency)])
  } else {
    AB$Agency[is.na(AB$Agency)] <- paste(agency, collapse = ", ")
  }

  # Handle County column
  if ("county" %in% colnames(AB)) {
    AB$County[is.na(AB$County)] <- ifelse(is.na(AB$county[is.na(AB$County)]),
                                          paste(unique(collections$county[collections$agency_code %in% agency]), collapse = ", "),
                                          AB$county[is.na(AB$County)])
  } else {
    counties <- unique(collections$county[collections$agency_code %in% agency])
    AB$County[is.na(AB$County)] <- paste(counties, collapse = ", ")
  }

  # Handle city column
  if ("city" %in% separate_by) {
    if ("city" %in% colnames(AB)) {
      AB$City[is.na(AB$City)] <- ifelse(is.na(AB$city[is.na(AB$City)]),
                                        "No City Data",
                                        AB$city[is.na(AB$City)])
    } else {
      AB$City <- "No city data"
    }
  }

  # Handle site_name column
  if ("site" %in% separate_by) {
    if ("site_name" %in% colnames(AB)) {
      AB$Site_Name[is.na(AB$Site_Name)] <- ifelse(is.na(AB$site_name[is.na(AB$Site_Name)]),
                                                  "No Site Data",
                                                  AB$site_name[is.na(AB$Site_Name)])
    } else {
      AB$Site_Name <- "No site data"
    }
  }

  # Handle Spatial column
  if ("spatial" %in% separate_by) {
    if ("spatial_feature" %in% colnames(AB)) {
      AB$Spatial[is.na(AB$Spatial)] <- ifelse(is.na(AB$spatial_feature[is.na(AB$Spatial)]),
                                              "No Spatial Data",
                                              AB$spatial_feature[is.na(AB$Spatial)])
    } else {
      AB$Spatial <- "No spatial data"
    }
  }

  # Calculate Abundance - handle division by zero
  AB$Abundance <- ifelse(AB$TrapEvents > 0,
                         round(AB$Count / AB$TrapEvents, 4),
                         0)
  AB <- AB %>% arrange(desc(surv_year), INTERVAL)

  # Select and rename columns - ADDED city and site_name to result columns
  result_cols <- c("Agency", "surv_year", "INTERVAL", "County")

  # ADDED: Include grouping columns in the final output
  if ("city" %in% separate_by) {
    result_cols <- c(result_cols, "City")
  }
  if ("site" %in% separate_by) {
    result_cols <- c(result_cols, "Site_Name")
  }
  if ("spatial" %in% separate_by) {
    result_cols <- c(result_cols, "Spatial")
  }

  result_cols <- c(result_cols, "Species", "Count", "TrapEvents", "Trap", "Abundance")

  # Select only columns that exist
  existing_cols <- result_cols[result_cols %in% colnames(AB)]
  AB <- AB %>%
    select(all_of(existing_cols))

  # Rename columns
  if ("INTERVAL" %in% colnames(AB)) {
    colnames(AB)[colnames(AB) == "INTERVAL"] <- interval
  }
  if ("surv_year" %in% colnames(AB)) {
    colnames(AB)[colnames(AB) == "surv_year"] <- "Year"
  }

  # Clean up the data - remove rows with NA TrapEvents but KEEP zero counts
  # Zero counts are valid (traps were set but nothing caught)
  result <- AB %>%
    filter(!is.na(TrapEvents),
           TrapEvents > 0) %>%
    distinct()

  return(result)
}

#' @title Calculate abundance
#'
#' @description Calculates abundance
#' @param  collections Collections data retrieved from getArthroCollections()
#' @param interval Calculation interval for abundance, accepts “collection_date”,“Biweek”,“Week”, and “Month.
#' @param species An optional vector for filtering species. Species_display_name is the accepted notation.To see a list of species present in your data run unique(collections$species_display_name). If species is unspecified, the default NULL will return data for all species in data.
#' @param trap An optional vector for filtering trap type by acronym. Trap_acronym is the is the accepted notation. Run unique(collections$trap_acronym) to see trap types present in your data. If trap is unspecified, the default NULL will return data for all trap types.
#' @param sex An optional vector for filtering sex type. Accepts 'male', 'female',or 'other'. If sex is unspecified, the default NULL will return data for female sex.
#' @param separate_by Separate/group the calculation by 'trap','species' or 'agency'. Default NULL does not separate.
#' @return A dataframe of abundance calculations.
#' @export
#' @importFrom dplyr across all_of
#' @examples
#' getAbundance(sample_collections,
#'              interval = 'Week',
#'              species = list('Cx pipiens'),
#'              trap = list('GRVD', 'CO2'),
#'              sex = list("female"),
#'              separate_by  = "species")
#' @export
#' @importFrom dplyr summarise summarize filter group_by distinct_at vars arrange mutate desc bind_rows rename
#' @importFrom tidyr pivot_wider pivot_longer any_of
#' @importFrom lubridate year month epiweek today
#' @importFrom utils tail



## Required: collections, interval
#  Collections data should be retrieved from getArthroCollections(...)
#interval to calculate abundance on, accepts "Week", "Biweek", "Month" where
#both Week and Biweek are epiweek and disease biweek.

##Optional: species, trap
#species, trap filter the data according to abbreviated scientific name and trap acronym
#If species, trap are left as NULL, the default assumes "All Options Selected"
#
getAbundance <- function(collections, interval, species = NULL, trap = NULL, sex = "female", separate_by = NULL) {

  if (nrow(collections) <= 0) {
    stop("Collections data is empty")
  }

  required_columns <- c("collection_id", "collection_date", "num_trap", "trap_nights",
                        "trap_problem_bit", "num_count", "sex_type", "species_display_name",
                        "trap_acronym")
  separate_options <- c("agency","species", "trap")

  if (any(!(required_columns %in% colnames(collections)))) {
    stop("Insufficient collections data provided")
  }
  if(any(!separate_by %in% separate_options)){
    stop("Check separate_by parameters. Accepted options are 'species', 'trap', and/or 'agency'")
  }

  collections <- collections %>%
    dplyr::filter(trap_nights != 0, num_trap != 0, trap_problem_bit == FALSE)

  if (!interval %in% c("Week", "Biweek", "Month")) {
    stop("Incorrect interval input. Interval accepts inputs of 'Week', 'Biweek', or 'Month'")
  }

  collections$INTERVAL <- switch(interval,
                                 "Week" = as.numeric(epiweek(collections$collection_date)),
                                 "Biweek" = as.numeric(ceiling(epiweek(collections$collection_date) / 2)),
                                 "Month" = as.numeric(month(collections$collection_date)))

  if (is.null(species)) {
    species <- unique(collections$species_display_name)
  }
  if (is.null(trap)) {
    trap <- unique(collections$trap_acronym)
  }
  if (is.null(sex)) {
    sex <- unique(collections$sex_type)
  }

  # Handle dynamic grouping
  grouping_vars <- c("surv_year", "INTERVAL")
  grouping_vars_trap = grouping_vars
  if (!is.null(separate_by)) {
    if ("species" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "species_display_name")
    }
    if ("agency" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "agency_code")
      grouping_vars_trap <- c(grouping_vars_trap, "agency_code")

    }
    if ("trap" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "trap_acronym")
      grouping_vars_trap <- c(grouping_vars_trap, "trap_acronym")

    }

  }

  # Calculate Counts
  collections %>%
    dplyr::filter(species_display_name %in% species,
                  trap_acronym %in% trap,
                  sex_type %in% sex) %>%
    dplyr::group_by(across(all_of(grouping_vars))) %>%
    dplyr::summarise(Count = sum(num_count, na.rm = TRUE),
                     Species = paste(sort(unique(species_display_name)), collapse = ", "),
                     Agency = paste(sort(unique(agency_code)), collapse = ", "),
                     .groups = "drop") -> cts


  # Calculate Trap Events
  collections %>%
    dplyr::filter(trap_acronym %in% trap) %>%
    distinct_at(vars(collection_id), .keep_all = TRUE) %>%
    dplyr::group_by(across(all_of(grouping_vars_trap))) %>%
    dplyr::summarise(TrapEvents = sum(trap_nights * num_trap, na.rm = TRUE),
                     Trap = paste(sort(unique(trap_acronym)), collapse = ", "),
                     .groups = "drop") -> tns

 # names(cts)[names(cts) == 'trap_acronym'] <- 'Trap'

  # Merge Counts and Trap Events
  AB <- merge(cts, tns, by = grouping_vars_trap)

  # Calculate Abundance
  AB$Abundance <- round(AB$Count / AB$TrapEvents, 2)
  AB <- AB %>% arrange(desc(surv_year), INTERVAL)
  AB  = AB %>%
    select(Agency, surv_year, INTERVAL, Species,Count,TrapEvents, Trap, Abundance)
  # Rename columns
  colnames(AB)[3] <- interval
  colnames(AB)[2] <- "Year"

  return(AB)

}

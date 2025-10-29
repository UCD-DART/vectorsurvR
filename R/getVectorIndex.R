#' Calculate vector index
#'
#' @description
#' `getVectorIndex()` Calculates vector index from pools and collections data
#' @param collections Collections data retrieved from `getArthroCollections()`
#' @param pools  Pools data retrieved from `getPools()`
#' @param interval Calculation interval for vector index, accepts "CollectionDate","Biweek","Week", and "Month
#' @param target_disease The disease to calculate infection rate for–i.e. "WNV". Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`
#' @param pt_estimate The estimation type for infection rate. Options include: "mle","bc-"mle", "mir"
#' @param scale Constant to multiply infection rate, default is 1000
#' @param agency Character vector for filtering agency by character code
#' @param species Character vector for filtering species. View species in your data `unique(data$species_display_name)`. Defaults to all species if no selection
#' @param trap Character vector for filtering trap type by acronym. View trap types in your data`unique(data$trap_acronym`. Defaults to all trap types
#' @param sex Character vector for filtering sex type. View sex options `unique(data$sex_type`). Defaults to "female".
#' @param trapnight_min Minimum trap night restriction for calculation. Default is 1.
#' @param trapnight_max Maximum trap night restriction for calculation. Default is no restriction.
#' @param separate_by Separate/group the calculation by 'trap','species' or 'agency'. Default NULL does not separate.
#' @importFrom dplyr arrange
#' @importFrom stringr str_split str_c
#' @examples
#' getVectorIndex(collections=sample_collections,
#' pools=sample_pools, interval="Month", target_disease = "WNV", pt_estimate="mle")
#' @export
#' @return Dataframe containing the vector index calculation

getVectorIndex  = function(collections, pools, interval,
                           target_disease,
                           pt_estimate = 'bc-mle',
                           scale = 1000,
                           agency = NULL,
                           species=NULL,
                           trap =  NULL,
                           sex = NULL,
                           trapnight_min=1,
                           trapnight_max=NULL,
                           separate_by = NULL){

  if (nrow(pools) <= 0) {
    stop("Pools data is empty")
  }

  pools_columns <- c("id", "collection_date", "surv_year", "num_count", "sex_type",
                     "species_display_name", "trap_acronym", "test_target_acronym",
                     "test_status_name")

  if (any(!(pools_columns %in% colnames(pools)))) {
    stop("Insufficent pools data")
  }

  if(nrow(collections)<=0){
    stop("Collections data is empty")
  }

  collections_columns = c("collection_id",
                          "collection_date",
                          "agency_code",
                          "num_trap",
                          "trap_nights",
                          "trap_problem_bit",
                          "num_count",
                          "sex_type",
                          "species_display_name",
                          "trap_acronym")


  if(any(!(collections_columns %in% colnames(collections)))){
    stop("Insufficent collections data provided")
  }

  if(!identical(sort(unique(pools$surv_year)), sort(unique(collections$surv_year)))){
    return("Years in Pools and Collections data do not match. Years much match for function to operate.")
  }


  IR = getInfectionRate(pools, interval, target_disease, pt_estimate, scale,
                        agency, species, trap, sex, separate_by)
  AB = getAbundance(collections, interval, agency,
                    species,
                    trap,
                    sex,
                    trapnight_min,
                    trapnight_max,
                    separate_by)

  grouping_vars = c()
  if (!is.null(separate_by)) {
    if ("species" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "Species")
    }
    if ("agency" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "Agency")
    }
    if ("trap" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "Trap")
    }
  }

  VI = merge(AB, IR, by = c("Year", interval, grouping_vars), all=T, sort = T,
             suffixes = c("_AB", "_IR"))
  VI$VectorIndex = VI$Abundance * VI$InfectionRate

  # Helper function to combine columns
  combine_columns_rowwise <- function(data, col1, col2, new_col_name = "Combined") {
    if (!(col1 %in% colnames(data)) | !(col2 %in% colnames(data))) {
      # If one or both columns are missing, return the original data unchanged
      return(data)
    }

    data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        !!new_col_name := {
          # Extract values safely for the current row using pick()
          values1 <- if (!is.na(pick(all_of(col1)))) str_split(pick(all_of(col1)), ",")[[1]] else character(0)
          values2 <- if (!is.na(pick(all_of(col2)))) str_split(pick(all_of(col2)), ",")[[1]] else character(0)

          # Combine, remove duplicates, sort, and collapse into a single string
          unique(c(values1, values2)) %>% sort() %>% str_c(collapse = ",")
        }
      ) %>%
      dplyr::ungroup()
  }

  if ("Species_AB" %in% colnames(VI) & "Species_IR" %in% colnames(VI)) {
    VI = combine_columns_rowwise(VI, "Species_AB", "Species_IR", new_col_name = "Species")
  }

  if ("Trap_AB" %in% colnames(VI) & "Trap_IR" %in% colnames(VI)) {
    VI = combine_columns_rowwise(VI, "Trap_AB", "Trap_IR", new_col_name = "Trap")
  }

  if ("Agency_AB" %in% colnames(VI) & "Agency_IR" %in% colnames(VI)) {
    VI = combine_columns_rowwise(VI, "Agency_AB", "Agency_IR", new_col_name = "Agency")
  }

  VI$Disease = target_disease

  # Build column selection dynamically based on what exists
  base_cols <- c("Year", interval, "Count", "TrapEvents", "Abundance",
                 "Disease", "InfectionRate", "VectorIndex")

  # Add optional columns if they exist
  optional_cols <- c("LowerCI", "UpperCI", "Agency", "Species", "Trap")
  existing_optional <- optional_cols[optional_cols %in% colnames(VI)]

  # Combine and select
  final_cols <- c(base_cols, existing_optional)
  VI <- VI %>% select(all_of(final_cols))

  return(VI)
}

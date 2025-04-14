#' Calculate vector index
#'
#' @description
#' `getVectorIndex()`requires at least five years prior to the target_year of arthro collections data to calculate for the specified parameters. The function uses the methods of the Gateway Abundance Anomaly calculator, and will not work if there is fewer than five years of data present.
#' @param collections Collections data retrieved from `getArthroCollections()`
#' @param pools  Pools data retrieved from `getPools()`
#' @param interval Calculation interval for vector index, accepts “collection_date”,“Biweek”,“Week”, and “Month
#' @param target_disease The disease to calculate infection rate for–i.e. “WNV”. Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`
#' @param pt_estimate The estimation type for infection rate. Options include: “mle”,“bc-”mle”, “mir”
#' @param scale Constant to multiply infection rate, default is 1000
#' @param agency An optional vector for filtering agency by character code
#' @param species An optional vector for filtering species. Species_display_name is the accepted notation.To see a list of species present in your data run unique(collections$species_display_name). If species is unspecified, the default NULL will return data for all species in data.
#' @param trap An optional vector for filtering trap type by acronym. Trap_acronym is the is the accepted notation. Run unique(collections$trap_acronym) to see trap types present in your data. If trap is unspecified, the default NULL will return data for all trap types.
#' @param sex An optional vector for filtering sex type. Accepts 'male', 'female',or 'other'. If sex is unspecified, the default NULL will return data for female sex.
#' @param trapnight_min Minimum trap night restriction for calculation. Default is 1.
#' @param trapnight_max Maximum trap night restriction for calculation. Default is no restriction.
#' @param separate_by Separate/group the calculation by 'trap','species' or 'agency'. Default NULL does not separate.
#' @param wide Should the data be returned in wide/spreadsheet format
#' @importFrom dplyr arrange
#' @importFrom stringr str_split str_c
#' @examples
#' getVectorIndex(sample_collections, sample_pools, "Month", "WNV", "mle", wide = FALSE )
#' @export
#' @return Dataframe containing the vector index calculation

getVectorIndex  = function(collections, pools, interval,
                           target_disease,pt_estimate,
                           scale = 1000,
                           agency = NULL,
                           species=NULL,
                           trap =  NULL,
                           sex = NULL,
                           trapnight_min=1,
                           trapnight_max=NULL,
                           separate_by = NULL,
                           wide=FALSE){

  if (nrow(pools) <= 0) {
    stop("Pools data is empty")

  }

  pools_columns <- c("pool_id", "collection_date", "surv_year", "num_count", "sex_type", "species_display_name", "trap_acronym", "target_acronym", "status_name")

  if (any(!(pools_columns %in% colnames(pools)))) {
    stop("Insufficent pools data")
  }

  if(nrow(collections)<=0){
    stop("Collections data is empty")
  }

  collections_columns =c("collection_id",
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
                        agency, species, trap, sex,separate_by, wide = FALSE)
  AB = getAbundance(collections,interval,agency,
                    species,
                    trap,
                    sex,
                    trapnight_min,
                    trapnight_max,
                    separate_by)

  grouping_vars=c()
  if (!is.null(separate_by)) {
    if ("species" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "Species")
    }
    if ("agency" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "Agency")

    }
    if ("subregion" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "Subregion")

    }
    if ("trap" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "Trap")
    }

  }
 VI = merge(AB,IR, by = c("Year",interval,grouping_vars), all=T, sort = T, suffixes = c("_AB", "_IR"))
 VI$VectorIndex = VI$Abundance*VI$InfectionRate
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


  VI = VI %>%
    select(Year,
           interval,
           Agency,
           Species,
           Count,
           Trap,
           TrapEvents,
           Abundance,
           Disease,
           InfectionRate,
           LowerCI,
           UpperCI,
           VectorIndex)

  if(wide==TRUE){
   VI %>%
     pivot_wider(values_from = VectorIndex, names_from = Year, names_prefix = "Vector_Index_")->VI

 }
  return(VI)

}

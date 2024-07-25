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
#' @param species_list Species filter for calculating abundance. Species_display_name is the accepted notation. To see a list of species present in your data run `unique(pools$species_display_name)`. If species is unspecified, the default `NULL` will return data for all species in data.
#' @param trap_list Trap filter for calculating abundance. Trap_acronym is the is the accepted notation. Run `unique(pools$trap_acronym)` to see trap types present in your data. If trap_list is unspecified, the default `NULL` will return data for all trap types.
#' @examples
#' getVectorIndex(sample_collections, sample_pools, "Month", "WNV", "mle" )
#' @export
#' @return Dataframe containing the vector index calculation

getVectorIndex  = function(collections, pools, interval,
                           target_disease,pt_estimate,
                           scale = 1000,
                           species_list=NULL,
                           trap_list =  NULL){

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


  IR = getInfectionRate(pools, interval, target_disease, pt_estimate, scale, species_list, trap_list)
  AB = getAbundance(collections,interval,
                    species_list,
                    trap_list,
                    species_separate=FALSE)

 VI = merge(AB,IR, by = c(interval, "surv_year"))
 VI$VectorIndex = VI$Abundance*VI$Point_Estimate

  return(VI)

}

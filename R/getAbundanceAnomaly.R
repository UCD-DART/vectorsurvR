#' Get Abundance Anomaly
#' @description `getAbundanceAnomaly(...) `requires at least five years prior to the target_year of arthro collections data to calculate for the specified parameters. The function uses the methods of the Gateway Abundance Anomaly calculator, and will not work if there is fewer than five years of data present.
#' @param collections Collections data retrieved from `getArthroCollections()`
#' @param interval Calculation interval for abundance, accepts “collection_date”,“Biweek”,“Week”, and “Month
#' @param target_year Year to calculate analysis on. Collections data must have a year range of at least (target_year - 5, target_year)
#' @param  species_list Species filter for calculating abundance. Species_display_name is the accepted notation.To see a list of species present in your data run `unique(collections$species_display_name)`. If species is unspecified, the default NULL will return data for all species in data
#' @param trap_list Trap filter for calculating abundance. Trap_acronym is the is the accepted notation. Run `unique(collections$trap_acronym)` to see trap types present in your data. If trap_list is unspecified,the default NULL will return data for all trap types
#' @param species_seperate Should the species in species_list have abundance calculated separately? Setting to FALSE calculates the combined abundance. The same result can be performed by calculating on one species at the time description
#' @keywords abundance
#' @export
#' @return Abundance anomaly calculation
#' @examples
#' getAbundanceAnomaly(sample_collections,"Biweek",target_year=2020, species_list="Cx pipiens")
#'
getAbundanceAnomaly <- function(collections, interval, target_year,
                               species_list = NULL, trap_list = NULL, species_seperate=FALSE){


  if(nrow(collections)<=0){
    stop("Collections data is empty")
  }


  col_columns =c("collection_id",
                 "collection_date",
                 "num_trap",
                 "trap_nights",
                 "trap_problem_bit",
                 "num_count",
                 "sex_type",
                 "species_display_name",
                 "trap_acronym")


  if(any(!(col_columns%in%colnames(collections)))) {


    stop("Insufficent collections data provided")

  }

 #check for valid year input
  present_years = sort(unique(collections$surv_year))

 if(!(target_year %in% present_years)){
   stop("Target year not present in data.")
 }
 if(tail(present_years, n = 1)>target_year){

   collections = collections %>%
     dplyr::filter(surv_year <= target_year)

   warning("There are years greater than the target year in the data. These years will not be included in the anomaly calculation.")
 }


ab_data = getAbundance(collections,interval, species_list, trap_list,species_seperate)

  colnames(ab_data)[2] ="INTERVAL"

  if(species_seperate==T){
    ab_data %>%
      group_by(INTERVAL, species_display_name) %>%
      dplyr::filter(surv_year != target_year )%>%
      summarise(Five_Year_Avg =  mean(Abundance), Years_In_Average = paste(sort(unique(surv_year)), collapse = ",")) -> yr_int_average
    ab_data_yr = ab_data %>% dplyr::filter(surv_year == target_year)

    ab_av <- merge(ab_data_yr, yr_int_average, by=c("INTERVAL","species_display_name"))
  }
  else{
    ab_data %>%
      group_by(INTERVAL) %>%
      dplyr::filter(surv_year != target_year )%>%
      summarise(Five_Year_Avg =  mean(Abundance), Years_In_Average = paste(sort(unique(surv_year)), collapse = ","))  -> yr_int_average

    ab_data_yr = ab_data %>% dplyr::filter(surv_year == target_year)

    ab_av <- merge(ab_data_yr, yr_int_average, by="INTERVAL")
  }


  ab_av$Delta = round((ab_av$Abundance - ab_av$Five_Year_Avg)/ab_av$Five_Year_Avg,4)*100

  ab_av = ab_av %>%
    arrange("INTERVAL")

  colnames(ab_av)[1] = interval


  return(ab_av)
}


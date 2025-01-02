#' Get Abundance Anomaly
#' @description `getAbundanceAnomaly(...) `requires at least five years prior to the target_year of arthro collections data to calculate for the specified parameters. The function uses the methods of the Gateway Abundance Anomaly calculator, and will not work if there is fewer than five years of data present.
#' @param collections Collections data retrieved from `getArthroCollections()`
#' @param interval Calculation interval for abundance, accepts “collection_date”,“Biweek”,“Week”, and “Month
#' @param target_year Year to calculate analysis on. Collections data must have a year range of at least (target_year - 5, target_year)
#' @param species An optional vector for filtering species. Species_display_name is the accepted notation.To see a list of species present in your data run unique(collections$species_display_name). If species is unspecified, the default NULL will return data for all species in data.
#' @param trap An optional vector for filtering trap type by acronym. Trap_acronym is the is the accepted notation. Run unique(collections$trap_acronym) to see trap types present in your data. If trap is unspecified, the default NULL will return data for all trap types.
#' @param sex An optional vector for filtering sex type. Accepts 'male', 'female',or 'other'. If sex is unspecified, the default NULL will return data for female sex.
#' @param separate_by Separate/group the calculation by 'trap','species' or 'agency'. Default NULL does not separate.
#' @keywords abundance
#' @importFrom utils head
#' @export
#' @return Abundance anomaly calculation
#' @examples
#' getAbundanceAnomaly(sample_collections,"Biweek",target_year=2020, species="Cx pipiens")
#'
getAbundanceAnomaly <- function(collections, interval, target_year,
                               species = NULL, trap = NULL, sex="female", separate_by=NULL){


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

  separate_options <- c("agency","species", "trap")


  if(any(!(col_columns%in%colnames(collections)))) {

    stop("Insufficent collections data provided")

  }
  if(any(!separate_by %in% separate_options)){
    stop("Check separate_by parameters. Accepted options are 'species', 'trap', and/or 'agency'")
  }

 #check for valid year input
  present_years = sort(unique(collections$surv_year),decreasing = T)

 if(!(target_year %in% present_years)){
   stop("Target year not present in data.")
 }
 if(head(present_years, n = 1)>target_year){

   warning("There are years greater than the target year in the data. These years will not be included in the anomaly calculation.")
 }
  calculation_years = sort(present_years[which(present_years<= target_year)],decreasing = T)[1:6]
  collections = collections %>%
    dplyr::filter(surv_year %in% calculation_years)


ab_data = getAbundance(collections,interval, species, trap,sex, separate_by)

  colnames(ab_data)[3] ="INTERVAL"
  grouping_vars <- c("INTERVAL")
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

    ab_data %>%
      group_by(across(all_of(grouping_vars))) %>%
      dplyr::filter(Year != target_year )%>%
      summarise(FiveYearAvg =  mean(Abundance), YearsInAverage = paste(sort(unique(Year)), collapse = ",")) -> yr_int_average
    ab_data_yr = ab_data %>% dplyr::filter(Year == target_year)

    ab_av <- merge(ab_data_yr, yr_int_average, by=grouping_vars)

  # else{
  #   ab_data %>%
  #     group_by(INTERVAL) %>%
  #     dplyr::filter(surv_year != target_year )%>%
  #     summarise(Five_Year_Avg =  mean(Abundance), Years_In_Average = paste(sort(unique(surv_year)), collapse = ","))  -> yr_int_average
  #
  #   ab_data_yr = ab_data %>% dplyr::filter(surv_year == target_year)
  #
  #   ab_av <- merge(ab_data_yr, yr_int_average, by="INTERVAL")
  # }


  ab_av$Delta = round((ab_av$Abundance - ab_av$FiveYearAvg)/ab_av$FiveYearAvg,4)*100

  ab_av = ab_av %>%
    arrange("INTERVAL")

  colnames(ab_av)[1] = interval


  return(ab_av)
}



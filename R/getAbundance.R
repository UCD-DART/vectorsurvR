#' @title Calculate abundance
#'
#' @description Calculates abundance
#' @param  collections Collections data retrieved from getArthroCollections()
#' @param interval Calculation interval for abundance, accepts “collection_date”,“Biweek”,“Week”, and “Month.
#' @param species_list Species filter for calculating abundance. Species_display_name is the accepted notation.To see a list of species present in your data run unique(collections$species_display_name). If species is unspecified, the default NULL will return data for all species in data.
#' @param trap_list Trap filter for calculating abundance. Trap_acronym is the is the accepted notation. Run unique(collections$trap_acronym) to see trap types present in your data. If trap_list is unspecified, the default NULL will return data for all trap types.
#' @param species_separate Should the species in species_list have abundance calculated separately? Setting to FALSE calculates the combined abundance. The same result can be performed by calculating on one species at the time.
#' @return A dataframe of abundance calculations
#' @export
#' @examples
#'
#' getAbundance(sample_collections,
#'              interval = 'Week',
#'              species_list = list('Cx pipiens'),
#'              trap_list = list('GRVD', 'CO2'),
#'              species_separate = FALSE)
#' @export
#' @importFrom dplyr summarise summarize filter group_by distinct_at vars arrange mutate desc
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom lubridate year month epiweek today
#' @importFrom utils tail



##Required: collections, interval
#Collections data should be retrieved from getArthroCollections(...)
#interval to calculate abundance on, accepts "Week", "Biweek", "Month" where
#both Week and Biweek are epiweek and disease biweek.

##Optional: species_list, trap_list
#species_list, trap_list filter the data according to abbreviated scientific name and trap acronym
#If species_list, trap_list are left as NULL, the default assumes "All Options Selected"
#
getAbundance <- function(collections,interval, species_list = NULL, trap_list = NULL, species_separate=FALSE){
  # collections$collection_date=as.Date(collections[['collection_date']])



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

  collections = collections %>%
    dplyr::filter(trap_nights!=0, num_trap!=0,trap_problem_bit==FALSE)

  #check for valid year input
  present_years = sort(unique(collections$surv_year))


  if(!interval%in%c("Week","Biweek","Month")){
    stop("Incorrect interval input. Interval accepts inputs of 'Week','Biweek'or 'Month'")
  }
  collections$INTERVAL = switch(interval,
                                "Week"= as.numeric(epiweek(collections$collection_date)),
                                "Biweek"= as.numeric(ceiling(epiweek(collections$collection_date)/2)),
                                "Month"= as.numeric(month(collections$collection_date)))

  if(is.null(species_list)){
    species_list = unique(collections$species_display_name)
  }
  if(is.null(trap_list)){
    trap_list = unique(collections$trap_acronym)
  }

  if(species_separate==FALSE){
    #We want to filter for females in the case of counts so here we care about species and trap type
    collections %>% group_by(surv_year, INTERVAL) %>%
      dplyr::filter(species_display_name %in% species_list,
             trap_acronym %in% trap_list,
             sex_type == "female")%>%
      summarise(Count = sum(num_count, na.rm=T)) -> cts

    #Trap Nights should not be based off of any specific sex type or species unless because we are looking at ALL possible collection opportunities present at the setting. Filter for distinct collections
    collections%>%
      dplyr::filter(trap_acronym %in% trap_list)%>%
      distinct_at(vars(collection_id), .keep_all = TRUE)%>%
      group_by(surv_year,INTERVAL)%>%
      summarise(Trap_Events=sum(trap_nights*num_trap))->tns
  }


  if(species_separate==TRUE){
    #We want to filter for females in the case of counts so here we care about species and trap type
    collections %>% group_by(surv_year, INTERVAL, species_display_name) %>%
      dplyr::filter(species_display_name %in% species_list,
             trap_acronym %in% trap_list,
             sex_type == "female")%>%
      summarise(Count = sum(num_count, na.rm=T)) -> cts

    #Trap Nights should not be based off of any specific trap type or species unless because we are looking at ALL possible collection opportunities present at the setting. Filter for distinct collections
    collections%>%
      dplyr::filter(trap_acronym %in% trap_list)%>%
      group_by(surv_year,INTERVAL)%>%
      distinct_at(vars(collection_id), .keep_all = TRUE)%>%
      summarise(Trap_Events=sum(trap_nights*num_trap)) -> tns
  }

  AB <- merge(cts, tns, by = c("surv_year","INTERVAL"))

  AB$Abundance = round(AB$Count/AB$Trap_Events, 2)
  AB = AB %>% arrange(desc(surv_year), (INTERVAL))
  colnames(AB)[2] = interval
  parameters=paste("Species: ", species_list, "Trap: ", trap_list)
  #AB$Filters=parameters

  return(AB)
}

#' @title Get Pools Frequency Table
#' @description `getPoolsComparisionTable()` produces a frequency table for positive, negative, and pending pools counts by year and
#' species. The more years present in the data, the larger the table.
#' @param pools Pools data retrieved from `getPools()`
#' @param interval Calculation interval for comparison table, accepts “collection_date”,“Biweek”,“Week”, and “Month
#' @param target_disease The disease to calculate infection rate for–i.e. “WNV”.
#'  Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`
#' @param species_separate Should the pools comparison be split by species of each pool. Default is FALSE
#' @examples
#' getPoolsComparisionTable(sample_pools,
#'                          interval = "Biweek",
#'                          target_disease = "WNV",
#'                           species_separate = TRUE)
#' @export
#' @return Frequency table of for pools data
#' @importFrom dplyr count
#' @importFrom knitr kable
#' @importFrom kableExtra kbl
#' @importFrom DT datatable



#Produces a frequency table for positive and negative pools counts by year

getPoolsComparisionTable = function(pools, interval, target_disease, species_separate=FALSE){


  if(nrow(pools)<=0){
    stop("Pools data is empty")
  }
  pools_columns = c("pool_id", "collection_date",  "num_count", "species_display_name", "trap_acronym", "target_acronym", "status_name")

  if(FALSE %in% (pools_columns%in%colnames(pools))){

    stop("Insufficent pools data, the following columns are required for the infection rate calculatior: pool_id
collection_date, num_trap, trap_nights, num_count, sex_type, species_display_name, trap_acronym, target_acronym, status_name")

  }

  if(!(interval %in% c("Biweek","Week","Month"))|
     !(target_disease %in% c("WNV", "SLEV", "WEEV"))){


    stop("Invalid parameters provided. See ?getPoolsComparison for more information.")
  }


  pools$INTERVAL = switch(interval,
                          "Week"= as.numeric(epiweek(pools$collection_date)),
                          "Biweek"= as.numeric(ceiling(epiweek(pools$collection_date)/2)),
                          "Month"= as.numeric(month(pools$collection_date)))
  pools_status = pools %>%
    dplyr::filter(target_acronym==target_disease)%>%
    group_by(surv_year, INTERVAL) %>%
    count(status_name)%>%
    pivot_wider(id_cols = c(surv_year,INTERVAL),
                names_from = "status_name",
                values_from = "n",values_fill = 0)%>%
    mutate(Total = sum(Confirmed,Negative, na.rm=T))%>%
    mutate(`Percent Positive` = round((Confirmed/Total)*100,2))

  if(species_separate == T){
    pools_status = pools %>% dplyr::filter(target_acronym==target_disease)%>%
      group_by(surv_year, species_display_name, INTERVAL) %>%
      count(status_name)%>%
      pivot_wider(id_cols = c(surv_year,INTERVAL, species_display_name),
                  names_from = "status_name",
                  values_from = "n",values_fill = 0)%>%
      mutate(Total = sum(Confirmed,Negative, na.rm=T))%>%
      mutate(`Percent Positive` = round((Confirmed/Total)*100, 2))

  }

  colnames(pools_status)[1:2] = c("Year",interval)

  return(pools_status)
}




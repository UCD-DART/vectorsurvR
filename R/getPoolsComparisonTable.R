#' @title Get Pools Frequency Table
#' @description `getPoolsComparisionTable()` produces a frequency table for positive, negative, and pending pools counts by year and
#' species. The more years present in the data, the larger the table.
#' @param pools Pools data retrieved from `getPools()`
#' @param interval Calculation interval for comparison table, accepts “collection_date”,“Biweek”,“Week”, and “Month
#' @param target_disease The disease to calculate infection rate for–i.e. “WNV”.
#'  Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`
#' @param separate_by Separate/group the calculation by 'trap','species' or 'agency'. Default NULL does not separate.
#' @examples
#' getPoolsComparisionTable(sample_pools,
#'                          interval = "Biweek",
#'                          target_disease = "WNV",
#'                           separate_by = "species")
#' @export
#' @return Frequency table of for pools data
#' @importFrom dplyr count
#' @importFrom knitr kable
#' @importFrom kableExtra kbl
#' @importFrom DT datatable



#Produces a frequency table for positive and negative pools counts by year

getPoolsComparisionTable = function(pools, interval, target_disease, separate_by=NULL){


  if(nrow(pools)<=0){
    stop("Pools data is empty")
  }
  pools_columns = c("id", "collection_date",  "num_count", "species_display_name", "trap_acronym", "test_target_acronym", "test_status_name")

  if(FALSE %in% (pools_columns%in%colnames(pools))){

    stop("Insufficent pools data")

  }

  if(!(interval %in% c("Biweek","Week","Month"))|
     !(target_disease %in% c("WNV", "SLEV", "WEEV"))){


    stop("Invalid parameters provided. See ?getPoolsComparison for more information.")
  }


  pools$INTERVAL = switch(interval,
                          "Week"= as.numeric(epiweek(pools$collection_date)),
                          "Biweek"= as.numeric(ceiling(epiweek(pools$collection_date)/2)),
                          "Month"= as.numeric(month(pools$collection_date)))
  grouping_vars <- c("surv_year", "INTERVAL")
  if (!is.null(separate_by)) {
    if ("species" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "species_display_name")
    }
    if ("trap" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "trap_acronym")
    }
    if ("agency" %in% separate_by) {
      grouping_vars <- c(grouping_vars, "agency_code")
    }
  }

  pools_status = pools %>%
    dplyr::filter(test_target_acronym==target_disease)%>%
    group_by(across(all_of(grouping_vars))) %>%
    count(test_status_name)%>%
    pivot_wider(id_cols = c(grouping_vars),
                names_from = "test_status_name",
                values_from = "n",values_fill = 0)%>%
    mutate(Total = sum(Confirmed,Negative, na.rm=T))%>%
    mutate(`Percent Positive` = round((Confirmed/Total)*100,2))

  colnames(pools_status)[1:2] = c("Year",interval)

  return(pools_status)
}




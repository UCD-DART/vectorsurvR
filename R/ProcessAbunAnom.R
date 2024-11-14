#' @title Process abundance anomaly
#'
#' @description `processAbunAnom()` processes the output returned from `getAbundanceAnomaly()` into a long form suitable for plotting using 'ggplot'
#' @param AbAnomOutput output from `getAbunAnom()`
#' @export
#' @examples
#' AbAnOut = getAbundanceAnomaly(sample_collections,
#'                              interval = "Biweek",
#'                              target_year = 2020,
#'                              species_list = c("Cx tarsalis", "Cx pipiens"),
#'                              species_separate = TRUE)
#' AbAnOut_L = processAbunAnom(AbAnOut)
#' @return Abundance anomaly output processed into long form, used for plotting functions




##Takes five year Abundance Anomaly output and returns a processed form which can be handled by ggplot

processAbunAnom = function(AbAnomOutput){

  #colnames(AbAnomOutput)[grep("Abundance",colnames(AbAnomOutput), value=F)]=paste(AbAnomOutput$surv_year,"Abundance", sep="_")
  ab_name=list()
  ab_name = grep("Abundance",colnames(AbAnomOutput), value=T)

  AbAnomOutput_L = AbAnomOutput %>%
    pivot_longer(cols=c(ab_name,
                        "Five_Year_Avg",
                        "Delta"),
                 values_to = "Abundance_Calculation",
                 names_to = "Abundance_Type")




  return(AbAnomOutput_L)
}


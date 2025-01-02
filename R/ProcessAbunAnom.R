#' @title Process abundance anomaly
#'
#' @description `processAbunAnom()` processes the output returned from `getAbundanceAnomaly()` into a long form suitable for plotting using 'ggplot'
#' @param AbAnomOutput output from `getAbunAnom()`

#' @return Abundance anomaly output processed into long form, used for plotting functions




##Takes five year Abundance Anomaly output and returns a processed form which can be handled by ggplot

processAbunAnom = function(AbAnomOutput){

  #colnames(AbAnomOutput)[grep("Abundance",colnames(AbAnomOutput), value=F)]=paste(AbAnomOutput$surv_year,"Abundance", sep="_")
  ab_name=list()
  ab_name = grep("Abundance",colnames(AbAnomOutput), value=T)

  AbAnomOutput_L = AbAnomOutput %>%
    pivot_longer(cols=c(ab_name,
                        "FiveYearAvg",
                        "Delta"),
                 values_to = "AbundanceCalculation",
                 names_to = "AbundanceType")




  return(AbAnomOutput_L)
}


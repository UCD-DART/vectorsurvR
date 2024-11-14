#' @title Plot abundance anomaly rate
#' @description plotAbundanceAnomaly() plots the output returned from `getInfectionRate()` with confidence intervals using ggplot. Note the output must not be in wide format.
#' @param AbunAnomOutput Output from returned `getAbundanceAnomaly()`
#' @return ggplot object
#' @examples
#' AbunAnomOutput = getAbundanceAnomaly(sample_collections,"Biweek",target_year=2020)
#' plotAbundanceAnomaly(AbunAnomOutput)
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_path geom_point scale_x_continuous aes labs ggtitle facet_wrap scale_color_brewer
#' coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom stats qnorm



plotAbundanceAnomaly = function(AbunAnomOutput){


  if(nrow(AbunAnomOutput)<=0){
    stop("Input is empty")
  }

  output_columns =c("surv_year",

                         "Count",
                         "Trap_Events",
                         "Abundance",
                    "Five_Year_Avg",
                    "Years_In_Average",
                    "Delta")


  if(any(!(output_columns %in% colnames(AbunAnomOutput)))){


    stop("Input needs to be from getAbundanceAnomaly()")

  }
  AbAnOut_L = processAbunAnom(AbunAnomOutput)
  INTERVAL_name = colnames(AbAnOut_L)[1]
  colnames(AbAnOut_L)[1] = "INTERVAL"

  if(length(unique(AbunAnomOutput$species_display_name))>1){
  AbAnOut_L %>%  filter(!Abundance_Type %in% c("Delta"))  %>%
    ggplot(aes(x = INTERVAL,
               y = Abundance_Calculation,
               color = Abundance_Type)) +
    geom_point() +
    geom_line() +
      labs(title = paste("Abundance Anomaly", unique(AbunAnomOutput$surv_year),"for",unique(AbunAnomOutput$species_display_name)), x=INTERVAL_name,y = "Abundance")+
      facet_wrap(~species_display_name) +
      scale_color_brewer(palette = "Set1")
  }else{
    AbAnOut_L %>%  filter(!Abundance_Type %in% c("Delta"))  %>%
      ggplot(aes(x = INTERVAL,
                 y = Abundance_Calculation,
                 color = Abundance_Type)) +
      geom_point() +
      geom_path(size = 0.75) +
      labs(title = paste("Abundance Anomaly", unique(AbunAnomOutput$surv_year)), x=INTERVAL_name,y = "Abundance", color = "Legend")+
     scale_color_brewer(palette = "Set1")
}


}

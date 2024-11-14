#' @title Plot infection rate
#' @description plotInfectionRate() plots the output returned from `getInfectionRate()` with confidence intervals using ggplot. Note the output must not be in wide format.
#' @param InfRtOutput Output from returned `getInfectionRate()`
#' @param year Year to plot infection rate on
#' @return ggplot object
#' @examples
#' IR = getInfectionRate(sample_pools,
#'                       interval = "Week",
#'                       target_disease = "WNV",
#'                       pt_estimate = "mle", species_list = c("Cx pipiens"),
#'                       trap_list = c("CO2","GRVD") )
#' plotInfectionRate(InfRtOutput = IR, year = 2017)
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_path geom_point scale_x_continuous aes labs ggtitle
#' coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom stats qnorm



plotInfectionRate = function(InfRtOutput, year){

  if(!("surv_year" %in% colnames(InfRtOutput))){
    stop("Please ensure infection rate output is in long format, NOT wide format")
  }
  if(!(year %in% unique(InfRtOutput$surv_year))){
    stop("Year not present in output")
  }
  plot_data = InfRtOutput%>%
    dplyr::filter(surv_year==year)
  interval_name = colnames(plot_data)[2]
  colnames(plot_data)[2]="INTERVAL"
  plot_data %>%
    ggplot(aes(x = INTERVAL, y = Point_Estimate))+
    geom_point( color="navyblue")+
    geom_path()+
    geom_line(aes(INTERVAL, Lower_CI), color = "steelblue", size = 0.1) +
    geom_line(aes(INTERVAL, Upper_CI), color = "steelblue", size = 0.1) +
    geom_ribbon(aes(ymin=Lower_CI, ymax=Upper_CI), alpha=0.2, fill = "steelblue2", color=NA) +
    labs(x=interval_name, y="Point Estimate (MLE and 95% CI)")+
    ggtitle(paste(year,"WNV Infection Rate"))+
    scale_color_brewer(palette = "Set1")->IR_plot

  return(IR_plot)

}

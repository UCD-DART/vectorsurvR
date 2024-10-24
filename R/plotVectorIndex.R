#' @title Plot Vector Index
#' @description plotVectorIndex() plots the output returned from `getVectorIndex()` with confidence intervals using ggplot
#' @param VIOutput Output from returned `getVectorIndex()`
#' @param year Year to plot vector index on
#' @return ggplot object
#' @examples
#' VI = getVectorIndex(sample_collections,sample_pools,
#'                       interval = "Week",
#'                       target_disease = "WNV",
#'                       pt_estimate = "mle", species_list = c("Cx pipiens"),
#'                       trap_list = c("CO2","GRVD") )
#' plotVectorIndex(VIOutput = VI, year = 2017)
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_path geom_point scale_x_continuous aes labs ggtitle
#' coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom stats qnorm



plotVectorIndex = function(VIOutput, year){

  if(!(year %in% unique(VIOutput$surv_year))){
    stop("Year not present in output")
  }
  plot_data = VIOutput%>%
    dplyr::filter(surv_year==year)
  interval_name = colnames(plot_data)[1]
  colnames(plot_data)[1]="INTERVAL"
  plot_data %>%
    ggplot(aes(x = INTERVAL, y = VectorIndex))+
    geom_point( color="navyblue")+
    geom_path()+
    geom_line(aes(INTERVAL, Lower_CI*Abundance), color = "steelblue", size = 0.1) +
    geom_line(aes(INTERVAL, Upper_CI*Abundance), color = "steelblue", size = 0.1) +
    geom_ribbon(aes(ymin=Lower_CI*Abundance, ymax=Upper_CI*Abundance), alpha=0.2, fill = "steelblue2", color=NA) +
    labs(x=interval_name, y="Vector Index (95% CI)")+
    ggtitle(paste(year,"WNV Vector Index"))->VI_plot

  return(VI_plot)

}

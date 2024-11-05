#' @title Plot Vector Index
#' @description plotVectorIndex() plots the output returned from `getVectorIndex()` using ggplot
#' @param VIOutput Output from returned `getVectorIndex()`
#' @return ggplot object
#' @examples
#' VI = getVectorIndex(sample_collections,sample_pools,
#'                       interval = "Week",
#'                       target_disease = "WNV",
#'                       pt_estimate = "mle", species_list = c("Cx pipiens"),
#'                       trap_list = c("CO2","GRVD") )
#' plotVectorIndex(VIOutput = VI)
#' @export
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_path geom_point scale_x_continuous aes labs ggtitle
#' coord_cartesian
#' @importFrom plotly ggplotly
#' @importFrom stats qnorm



plotVectorIndex = function(VIOutput){


  if(!("surv_year" %in% colnames(VIOutput))){
    stop("Please ensure vector index output is in long format, NOT wide format")
  }
  interval_name = colnames(VIOutput)[1]
  colnames(VIOutput)[1]="INTERVAL"
  VIOutput %>%
    ggplot(aes(x = INTERVAL, y = Vector_Index, color=factor(surv_year)))+
    geom_point()+
    geom_path()+

    labs(x=interval_name, y="Vector Index", color = "Year")+
    ggtitle("WNV Vector Index")->VI_plot

  return(VI_plot)

}

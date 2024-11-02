#' @title Calculate infection rate
#'
#' @description `getInfectionRate()` requires at least five years prior to the target_year of arthro collections data to calculate for the specified parameters. The function uses the methods of the Gateway Abundance Anomaly calculator, and will not work if there is fewer than five years of data present.
#' @param pools  Pools data retrieved from `getPools()`
#' @param interval Calculation interval for infection rate, accepts “collection_date”,“Biweek”,“Week”, and “Month
#' @param target_disease The disease to calculate infection rate for–i.e. “WNV”. Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`
#' @param pt_estimate The estimation type for infection rate. Options include: “mle”,“bc-mle”, “mir”
#' @param scale Constant to multiply infection rate by
#' @param species_list Species filter for calculating infection rate species_display_name is the accepted notation. To see a list of species present in your data run `unique(pools$species_display_name)`. If species is unspecified, the default `NULL` will return data for all species in data.
#' @param trap_list Trap filter for calculating infection rate. Trap_acronym is the is the accepted notation. Run `unique(pools$trap_acronym)` to see trap types present in your data. If trap_list is unspecified, the default `NULL` will return data for all trap types.
#' @param wide Should the data be returned in wide/spreadsheet format
#' @keywords pools infection rate
#' @return Dataframe of infection rate calculation
#' @importFrom tidyr pivot_wider
#' @examples
#' getInfectionRate(sample_pools,
#'                  interval = "Biweek",
#'                  target_disease = "WNV",
#'                  pt_estimate = "mle",
#'                  scale = 1000,
#'                  species_list = list("Cx pipiens"),
#'                  trap_list = list("CO2"),
#'                  wide = FALSE)
#' @export



#Requires pools data from getPools
#interval to calculate abundance on, accepts "Week", "Biweek", "Month" where both Week and Biweek are epiweek and disease biweek.
#target_year: the year to calculate the infection rate on. The function will check if 5 past years of data are present in provided data
#target_disease: accepts disease acronyms: "WNV", "SLEV",etc unique(pools$trap_acronym) will give a list of all options
#pt_estimate: "mle","bc-mle", "mir"
##Optional: species_list, trap_list
#species_list, trap_list filter the data according to abbreviated scientific name (Cx pipiens etc) and trap acronym (NJLT, CO2, GRVD...)
#If species_list, trap_list are left as NULL, the default assumes "All Options Selected"

getInfectionRate <- function(pools, interval, target_disease, pt_estimate, scale = 1000, species_list = NULL, trap_list = NULL, wide = FALSE) {

  if (nrow(pools) <= 0) {
    stop("Pools data is empty")

    }

  pools_columns <- c("pool_id", "collection_date", "surv_year", "num_count", "sex_type", "species_display_name", "trap_acronym", "target_acronym", "status_name")

  if (!all(pools_columns %in% colnames(pools))) {
    stop("Insufficent pools data")
  }

  valid_intervals <- c("Biweek", "Week", "Month")
  valid_diseases <- c("WNV", "SLEV", "WEEV")
  valid_pt_estimates <- c("mle", "bc-mle", "mir")

  if (any(!(interval %in% valid_intervals), !(target_disease %in% valid_diseases), !(pt_estimate %in% valid_pt_estimates))) {
    stop("Invalid parameters. See documentation for getInfectionRate()")
  }

  # Logic to prevent misinformation. If a species or trap is not in the data, the user is notified.
  if (!is.null(species_list) & any(!(species_list %in% unique(pools$species_display_name)))) {
    warning("A species in species_list is not present in pools data. See ?getInfectionRate for more information")
  }

  if (!is.null(trap_list) & FALSE %in% c(trap_list %in% unique(pools$trap_acronym))) {
    warning("A trap in trap_list not present in pools data. See ?getInfectionRate for more information")
  }

  pools$collection_date <- as.Date(pools$collection_date)

  pools$INTERVAL <- switch(interval,
                           "Week" = as.numeric(epiweek(pools$collection_date)),
                           "Biweek" = as.numeric(ceiling(epiweek(pools$collection_date) / 2)),
                           "Month" = as.numeric(month(pools$collection_date)))

  if (is.null(species_list)) {
    species_list <- unique(pools$species_display_name)
  }

  if (is.null(trap_list)) {
    trap_list <- unique(pools$trap_acronym)
  }

  pools$status_name <- ifelse(pools$status_name == "Confirmed", 1, 0)

  ##Calculations

  #Min infection rate-> simply ratio calculation, assumes one infected mosquito per postive pool
  #confidence interval derived
  mir <-
    function(x,m,n=rep(1,length(x)), alpha=0.05, scale)
    {
      N <- sum(m*n)
      mir <- sum(x)/N
      mir_stderr <- sqrt(mir*(1-mir)/N)
      z <- qnorm(1-alpha/2)
      return(c(p=mir*scale,lower=max(0, mir - z * mir_stderr)*scale, upper = min(1, mir + z * mir_stderr)*scale))
    }

  mle <-
    function(x, m, n = rep(1., length(x)), tol = 1e-008, alpha=0.05, scale)
    {
      #
      # This is the implementation using Newton-Raphson, as given
      # in the Walter, Hildreth, Beaty paper, Am. J. Epi., 1980
      #
      if(length(m) == 1.) m <- rep(m, length(x)) else if(length(m) != length(x))
        stop("\n ... x and m must have same length if length(m) > 1")
      if(any(x > n))
        stop("x elements must be <= n elements")
      if(all(x == 0.))
        return(c(p = 0, lower = 0, upper =0))
      if(sum(x) == sum(n)) return(NA)
      p_new <- 1 - (1 - sum(x)/sum(n))^(1/mean(m)) # starting value
      done <- 0
      N <- sum(n * m)
      while(!done) {
        p_old <- p_new
        p_new <- p_old - (N - sum((m * x)/(1 - (1 - p_old)^m)))/
          sum((m^2 * x * (1 - p_old)^(m - 1))/(1 - (1 -
                                                      p_old)^m)^2)
        if(abs(p_new - p_old) < tol) #tolerace
          done <- 1
      }
      p_hat = p_new

      if(length(m) == 1)
        m <- rep(m, length(x))

      if(p_hat > 0 & p_hat < 1){
        p_stderr <- sqrt(1/sum((m^2 * n * (1 - p_hat)^(m - 2))/(1 - (1 - p_hat)^m)))}
      else{p_stderr<-0}

      z <- qnorm(1 - alpha/2)
      return(c(p = p_hat*scale, lower = max(0, p_hat - z * p_stderr)*scale, upper = min(1, p_hat +
                                                                       z * p_stderr)*scale))

    }
  bcmle <- function(x, m, n = rep(1., length(x)), tol = 1e-008, alpha=0.05, scale)
  {
    #
    # This is the implementation using Newton-Raphson, as given
    # in the Walter, Hildreth, Beaty paper, Am. J. Epi., 1980
    #
    if(length(m) == 1.) m <- rep(m, length(x)) else if(length(m) != length(x))
      stop("\n ... x and m must have same length if length(m) > 1")
    if(any(x > n))
      stop("x elements must be <= n elements")
    if(all(x == 0.))
      return(c(p = 0, lower = 0, upper =0))
    if(sum(x) == sum(n)) return(NA)
    p_new <- 1 - (1 - sum(x)/sum(n))^(1/mean(m)) # starting value
    done <- 0
    N <- sum(n * m)
    while(!done) { #updates p_hat based on
      p_old <- p_new
      p_new <- p_old - (N - sum((m * x)/(1 - (1 - p_old)^m)))/
        sum((m^2 * x * (1 - p_old)^(m - 1))/(1 - (1 -
                                                    p_old)^m)^2)
      if(abs(p_new - p_old) < tol)
        done <- 1
    }
    p_hat = p_new

    if(length(m) == 1)
      m <- rep(m, length(x))

    if(p_hat > 0 ){

      bias <- sum((m-1)*m^2*n*(1-p_hat)^(m-3)/(1-(1-p_hat)^m))*(((1/sum((m^2 * n * (1 - p_hat)^(m - 2))/(1 - (1 - p_hat)^m))))^2)/2}
    else{bias<-0}
    p_hat = p_hat-bias

    p_stderr <- sqrt(1/sum((m^2 * n * (1 - p_hat)^(m - 2))/(1 - (1 - p_hat)^m)))
    z <- qnorm(1 - alpha/2)
    return(c(p = p_hat*scale, lower = max(0, p_hat - z * p_stderr)*scale, upper = min(1, p_hat +
                                                                            z * p_stderr)*scale))

  }

 #}
  # Function to filter data accordingly
  check <- function(inter, year, target_disease, pt_estimate, species_list, trap_list) {
    data <- pools %>% dplyr::filter(surv_year == year &
                                      INTERVAL == inter &
                                      species_display_name %in% species_list &
                                      trap_acronym %in% trap_list &
                                      target_acronym == target_disease)

    # Calculate the infection rate point estimate
    if (nrow(data) > 0) {
      if(pt_estimate == "mir"){
        ir <- mir(data$status_name, data$num_count, scale = scale)}
      if(pt_estimate == "mle"){
        ir <- mle(data$status_name, data$num_count, scale = scale)}
      if(pt_estimate == "bc-mle"){
        ir <- bcmle(data$status_name, data$num_count, scale = scale)
      }


      species = paste(sort(unique(data$species_display_name)), collapse = ",")
      traps = paste(sort(unique(data$trap_acronym)), collapse = ",")
      return(c(year, inter, target_disease, ir,species, traps ))

    }

    }


  # Build a data frame of the point estimates for IR for all elements in data
  IR <- c()

  for (year in sort(unique(pools$surv_year))) {
    for (inter in sort(unique(pools$INTERVAL))) {
      IR <- rbind(IR, (check(inter, year, target_disease, pt_estimate, species_list, trap_list)))
    }
  }

  colnames(IR) <- c("surv_year", interval, "Disease", "Point_Estimate", "Lower_CI", "Upper_CI", "Species", "Trap_Type")
  IR <- as.data.frame(IR)
  IR[c(2, 4:6)] <- sapply(IR[c(2, 4:6)], as.numeric)

if(wide==TRUE){
  IR %>%
    pivot_wider(values_from = Point_Estimate, names_from = surv_year, names_prefix = "IR_Estimate_")->IR
}

  return(IR)
}

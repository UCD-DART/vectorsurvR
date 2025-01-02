#' @title Calculate infection rate
#'
#' @description `getInfectionRate()` requires at least five years prior to the target_year of arthro collections data to calculate for the specified parameters. The function uses the methods of the Gateway Abundance Anomaly calculator, and will not work if there is fewer than five years of data present.
#' @param pools  Pools data retrieved from `getPools()`
#' @param interval Calculation interval for infection rate, accepts “collection_date”,“Biweek”,“Week”, and “Month
#' @param target_disease The disease to calculate infection rate for–i.e. “WNV”. Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`
#' @param pt_estimate The estimation type for infection rate. Options include: “mle”,“bc-mle”, “mir”
#' @param scale Constant to multiply infection rate by
#' @param species An optional vector for filtering species. Species_display_name is the accepted notation.To see a list of species present in your data run unique(collections$species_display_name). If species is unspecified, the default NULL will return data for all species in data.
#' @param trap An optional vector for filtering trap type by acronym. Trap_acronym is the is the accepted notation. Run unique(collections$trap_acronym) to see trap types present in your data. If trap is unspecified, the default NULL will return data for all trap types.
#' @param sex An optional vector for filtering sex type. Accepts 'male', 'female',or 'other'. If sex is unspecified, the default NULL will return data for female sex.
#' @param separate_by Separate/group the calculation by 'trap','species' or 'agency'. Default NULL does not separate.
#' @param wide Should the data be returned in wide/spreadsheet format
#' @keywords pools infection rate
#' @return Dataframe of infection rate calculation
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select case_when
#' @importFrom stats qnorm
#' @export

getInfectionRate <- function(pools, interval, target_disease, pt_estimate, scale = 1000,
                             species = NULL, trap = NULL, sex = "female", separate_by = NULL, wide = FALSE) {

  if (nrow(pools) <= 0) {
    stop("Pools data is empty")
  }

  pools_columns <- c("pool_id", "collection_date", "surv_year", "num_count", "sex_type",
                     "species_display_name", "trap_acronym", "target_acronym", "status_name")

  if (!all(pools_columns %in% colnames(pools))) {
    stop("Insufficient pools data")
  }

  valid_intervals <- c("Biweek", "Week", "Month")
  valid_diseases <- c("WNV", "SLEV", "WEEV")
  valid_pt_estimates <- c("mle", "bc-mle", "mir")
  valid_separate_by <- c("species", "trap", "agency")

  if (!interval %in% valid_intervals ||
      !target_disease %in% valid_diseases ||
      !pt_estimate %in% valid_pt_estimates) {
    stop("Invalid parameters. See documentation for getInfectionRate()")
  }

  if (!is.null(separate_by) && any(!separate_by %in% valid_separate_by)) {
    stop("Invalid 'separate_by' parameter. Accepted values are 'species', 'trap', and 'agency'.")
  }

  # Convert collection_date to Date format
  pools$collection_date <- as.Date(pools$collection_date)

  # Create time intervals
  pools$INTERVAL <- switch(interval,
                           "Week" = as.numeric(epiweek(pools$collection_date)),
                           "Biweek" = as.numeric(ceiling(epiweek(pools$collection_date) / 2)),
                           "Month" = as.numeric(month(pools$collection_date)))

  # Default species and trap if not provided
  if (is.null(species)) {
    species <- unique(pools$species_display_name)
  }
  if (is.null(trap)) {
    trap <- unique(pools$trap_acronym)
  }
  if (is.null(sex)) {
    sex <- unique(pools$sex_type)
  }
  # Binary status for infection (Confirmed = 1, else 0)
  pools$status_name <- ifelse(pools$status_name == "Confirmed", 1, 0)
  # Dynamic grouping variables
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

  # Filter and group data

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



  # Apply infection rate calculation based on the chosen pt_estimate method
       results <- pools %>%
         dplyr::filter(
           species_display_name %in% species,
           trap_acronym %in% trap,
           sex_type %in% sex,
           target_acronym == target_disease
         ) %>%
         dplyr::group_by(across(all_of(grouping_vars))) %>%
         dplyr::summarise(Agency = paste(sort(unique(agency_code)), collapse = ", "),
                          Species = paste(sort(unique(species_display_name)), collapse = ", "),
                          Trap = paste(sort(unique(trap_acronym)), collapse = ", "),
                          Disease = paste(sort(unique(target_acronym)), collapse = ", "),


           # Apply infection rate functions directly to vectors
           InfectionRate = case_when(
             pt_estimate == "mir" ~ mir(status_name, num_count, scale = scale)[1],
             pt_estimate == "mle" ~ mle(status_name, num_count, scale = scale)[1],
             pt_estimate == "bc-mle" ~ bcmle(status_name, num_count, scale = scale)[1]
           ),
           LowerCI = case_when(
             pt_estimate == "mir" ~ mir(status_name, num_count, scale = scale)[2],
             pt_estimate == "mle" ~ mle(status_name, num_count, scale = scale)[2],
             pt_estimate == "bc-mle" ~ bcmle(status_name, num_count, scale = scale)[2]
           ),
           UpperCI = case_when(
             pt_estimate == "mir" ~ mir(status_name, num_count, scale = scale)[3],
             pt_estimate == "mle" ~ mle(status_name, num_count, scale = scale)[3],
             pt_estimate == "bc-mle" ~ bcmle(status_name, num_count, scale = scale)[3]
           ) # Drop grouping after summarization
         )
    end = dim(results)[2]
    colnames(results)[c(1,2,end-2,end-1,end)] =  c("Year",interval,"InfectionRate", "LowerCI", "UpperCI")
# remove redundant columns
results$species_display_name <- NULL
results$trap_acronym <- NULL
results$agency_code <- NULL

  # If wide format is requested, pivot the results
  if (wide) {
    results <- results %>%
      tidyr::pivot_wider(
        names_from = Year,
        values_from = c(InfectionRate, LowerCI, UpperCI),
        names_prefix = "Year_"
      )
  }

  return(results)

}

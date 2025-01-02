#' @title Sample Mosquito Collections Data
#' @description Sample Mosquito Collections data imitates the essential components of real mosquito collections data
#' @format A data frame with 2500 rows and 13 variables:
#' \describe{
#'  \item{\code{agency_code}}{character Four letter agency code}
#'   \item{\code{collection_id}}{double Collection identification number}
#'   \item{\code{collection_date}}{character The date the trap was picked up for collection}
#'   \item{\code{num_trap}}{integer The number of unique traps in operation at one site}
#'   \item{\code{site_code}}{integer Identifying code of site}
#'   \item{\code{surv_year}}{double Surveillance year of collection}
#'   \item{\code{trap_nights}}{integer The number of nights a trap was in the field}
#'   \item{\code{trap_problem_bit}}{logical If these was an issue with the trap}
#'   \item{\code{num_count}}{integer Number of arthropods present in collection}
#'   \item{\code{sex_type}}{character Sex of collected arthropods}
#'   \item{\code{species_display_name}}{character Species name of collected arthropods}
#'   \item{\code{trap_acronym}}{character The acronym of the trap placed in the field}
#'}
#' @source \url{https://vectorsurv.org/}
"sample_collections"

#' @title Sample Pools Data
#' @description Sample Pools data imitates the essential components of real mosquito pools data needed for calculations
#' @format A data frame with 2500 rows and 10 variables:
#' \describe{
#' \item{\code{agency_code}}{character Four letter agency code}
#'   \item{\code{pool_id}}{integer Pool identification number}
#'   \item{\code{surv_year}}{integer Surveillance year of pool}
#'   \item{\code{site_code}}{integer Identifying code of site}
#'   \item{\code{collection_date}}{character The date the trap was picked up for collection}
#'   \item{\code{sex_type}}{integer Sex type of collected arthropods}
#'   \item{\code{num_count}}{integer Number of arthropods present in collection}
#'   \item{\code{target_acronym}}{character The disease being tested for in the pool}
#'   \item{\code{method_name}}{character Method used to test pool for disease}
#'   \item{\code{status_name}}{character Status of the tested disease, confirmed or negative}
#'   \item{\code{trap_acronym}}{character The acronym of the trap placed in the field}
#'   \item{\code{species_display_name}}{character Species name of collected arthropods}
#'}
#' @source \url{https://vectorsurv.org/}
"sample_pools"

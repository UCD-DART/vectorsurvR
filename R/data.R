#' @title Sample Mosquito Collections Data
#' @description Sample Mosquito Collections data imitates the essential components of real mosquito collections data
#' @format A data frame with 5880 rows and 17 variables:
#' \describe{
#'  \item{\code{agency_code}}{character Four letter agency code}
#'  \item{\code{agency_id}}{interger Unique agency id number}
#'  \item{\code{county}}{character County which site resides}
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
#'   \item{\code{collection_longitude}}{numeric longitude of collection}
#'   \item{\code{collection_latitude}}{numeric latitude of collection}
#'   \item{\code{spatial_feature}}{character name of spatial feature to which data belongs}
#'   \item{\code{multiple_features}}{bolean T/F if the point is found within mutiple selected spatial features}

#'}
#' @source \url{https://vectorsurv.org/}
"sample_collections"

#' @title Sample Pools Data
#' @description Sample Pools data imitates the essential components of real mosquito pools data needed for calculations
#' @format A data frame with 3500 rows and 15 variables:
#' \describe{
#' \item{\code{agency_code}}{character Four letter agency code}
#'  \item{\code{agency_id}}{interger Unique agency id number}
#'   \item{\code{id}}{integer Pool identification number}
#'   \item{\code{surv_year}}{integer Surveillance year of pool}
#'   \item{\code{site_code}}{integer Identifying code of site}
#'   \item{\code{collection_date}}{character The date the trap was picked up for collection}
#'   \item{\code{sex_type}}{integer Sex type of collected arthropods}
#'   \item{\code{num_count}}{integer Number of arthropods present in collection}
#'   \item{\code{test_target_acronym}}{character The disease being tested for in the pool}
#'   \item{\code{test_method_name}}{character Method used to test pool for disease}
#'   \item{\code{test_status_name}}{character Status of the tested disease, confirmed or negative}
#'   \item{\code{trap_acronym}}{character The acronym of the trap placed in the field}
#'   \item{\code{species_display_name}}{character Species name of collected arthropods}
#'   \item{\code{pool_longitude}}{numeric longitude of pool}
#'   \item{\code{pool_latitude}}{numeric latitude of pool}
#'}
#' @source \url{https://vectorsurv.org/}
"sample_pools"


#' @title Sample Spatial Data
#' @description Sample Spatial data imitates spatial feature data
#' @format A data frame with 3 rows and 5 variables:
#' \describe{
#'  \item{\code{agency}}{character Agency name}
#'  \item{\code{agency_id}}{interger Unique agency id number}

#' \item{\code{id}}{interger ID of spatial feature}
#'   \item{\code{name}}{character Name of spatial feature}
#'   \item{\code{geometry}}{multipolygon shape geometry spatal feature}
#'}
#' @source \url{https://vectorsurv.org/}
"sample_spatial"

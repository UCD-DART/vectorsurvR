% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sample_pools}
\alias{sample_pools}
\title{Sample Pools Data}
\format{
A data frame with 2500 rows and 10 variables:
\describe{
\item{\code{agency_code}}{character Four letter agency code}
  \item{\code{pool_id}}{integer Pool identification number}
  \item{\code{surv_year}}{integer Surveillance year of pool}
  \item{\code{site_code}}{integer Identifying code of site}
  \item{\code{collection_date}}{character The date the trap was picked up for collection}
  \item{\code{sex_type}}{integer Sex type of collected arthropods}
  \item{\code{num_count}}{integer Number of arthropods present in collection}
  \item{\code{target_acronym}}{character The disease being tested for in the pool}
  \item{\code{method_name}}{character Method used to test pool for disease}
  \item{\code{status_name}}{character Status of the tested disease, confirmed or negative}
  \item{\code{trap_acronym}}{character The acronym of the trap placed in the field}
  \item{\code{species_display_name}}{character Species name of collected arthropods}
  \item{\code{pool_longitude}}{numeric longitude of pool}
  \item{\code{pool_latitude}}{numeric latitude of pool}
}
}
\source{
\url{https://vectorsurv.org/}
}
\usage{
sample_pools
}
\description{
Sample Pools data imitates the essential components of real mosquito pools data needed for calculations
}
\keyword{datasets}

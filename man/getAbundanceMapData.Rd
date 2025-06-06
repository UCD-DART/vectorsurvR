% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAbundanceMapData.R
\name{getAbundanceMapData}
\alias{getAbundanceMapData}
\title{Get Abundance Map Data}
\usage{
getAbundanceMapData(collections, spatial, interval, selected_features)
}
\arguments{
\item{collections}{A dataframe containing collection coordinates}

\item{spatial}{A dataframe containing spatial data corresponding to the collections}

\item{interval}{Calculation interval for abundance, accepts“Biweek”,“Week”, and “Month}

\item{selected_features}{A character vector of spatial feature names to filter by}
}
\value{
A dataframe of collections with associated spatial regions
}
\description{
`getAbundanceMapData()` retrieves spatial feature data and associates it with collection locations.
}





checkVI <- function(VI) {
  expect_true(!is.null(VI))
  expect_s3_class(VI, "data.frame")
  expect_contains(colnames(VI), c("Year", "Disease","Count", "TrapEvents", "Abundance", "InfectionRate","Trap", "VectorIndex"))
}


test_that("Valid data returns a data frame formatted properly", {
  checkVI(getVectorIndex(collections = testing_collections, pools = testing_pools, interval = "Week",species =  NULL, trap = NULL, target_disease = "WNV", pt_estimate = "mir"))
  checkVI(getVectorIndex(collections = testing_collections, pools = testing_pools, interval = "Biweek", species = NULL, trap = NULL, target_disease = "SLEV", pt_estimate = "mir", separate_by="agency"))
  checkVI(getVectorIndex(collections = testing_collections, pools = testing_pools, interval = "Month", species = "Cx pipiens", trap = "GRVD", target_disease = "WNV",pt_estimate = "bc-mle",separate_by="trap"))
  checkVI(getVectorIndex(collections = testing_collections, pools = testing_pools, interval = "Month", species = c("Cx pipiens", "Cx tarsalis"), trap = c("CO2","GRVD"), target_disease = "SLEV", pt_estimate = "mle"))
  checkVI(getVectorIndex(collections = testing_collections, pools = testing_pools, interval = "Biweek", species = NULL, trap = "CO2", target_disease = "SLEV",pt_estimate = "mle"))
})

test_that("Error thrown when incorrect collections data",{

  expect_error(getVectorIndex(testing_collections[c(1:3)], testing_pools, interval = "Week",target_disease = "WNV",pt_estimate = "mle"), "Insufficent collections data provided")
})

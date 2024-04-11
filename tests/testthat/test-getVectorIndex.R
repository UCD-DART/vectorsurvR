



checkVI <- function(VI) {
  expect_true(!is.null(VI))
  expect_s3_class(VI, "data.frame")
  expect_contains(colnames(VI), c("surv_year", "Disease","Count", "Trap_Events", "Abundance", "Point_Estimate", "Lower_CI", "Upper_CI"))
}


test_that("Valid data returns a data frame formatted properly", {
  checkVI(getVectorIndex(collections = sample_collections, pools = sample_pools, interval = "Week",species_list =  NULL, trap_list = NULL, target_disease = "WNV", pt_estimate = "mir"))
  checkVI(getVectorIndex(collections = sample_collections, pools = sample_pools, interval = "Biweek", species_list = NULL, trap_list = NULL, target_disease = "SLEV", pt_estimate = "mir"))
  checkVI(getVectorIndex(collections = sample_collections, pools = sample_pools, interval = "Month", species_list = "Cx pipiens", trap_list = "GRVD", target_disease = "WNV",pt_estimate = "bc-mle"))
  checkVI(getVectorIndex(collections = sample_collections, pools = sample_pools, interval = "Month", species_list = c("Cx pipiens", "Cx tarsalis"), trap_list = c("CO2","GRVD"), target_disease = "SLEV", pt_estimate = "mle"))
  checkVI(getVectorIndex(collections = sample_collections, pools = sample_pools, interval = "Biweek", species_list = NULL, trap_list = "CO2", target_disease = "SLEV",pt_estimate = "mle"))
})

test_that("Error thrown when incorrect collections data",{

  expect_error(getVectorIndex(sample_collections[c(1:3)], sample_pools, interval = "Week",target_disease = "WNV",pt_estimate = "mle"), "Insufficent collections data provided")
})

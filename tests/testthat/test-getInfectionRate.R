



# Helper function to check IR output is valid
checkIR <- function(IR) {
  expect_true(!is.null(IR))
  expect_s3_class(IR, "data.frame")
  expect_contains(colnames(IR), c("surv_year", "Disease", "Point_Estimate", "Lower_CI", "Upper_CI"))
}

# Test cases
test_that("Valid parameters returns a data frame formatted properly", {
  checkIR(getInfectionRate(sample_pools, interval = "Week", target_disease = "WNV", pt_estimate = "bc-mle",scale = 1000, NULL, NULL))
  checkIR(getInfectionRate(sample_pools, interval = "Biweek", target_disease = "WNV", pt_estimate = "mle",scale = 1000, list("Cx pipiens","Cx tarsalis"), NULL))
  checkIR(getInfectionRate(sample_pools, interval = "Month", target_disease = "WNV", pt_estimate = "mir",scale = 1000, NULL, c("GRVD")))
  checkIR(getInfectionRate(sample_pools, interval = "Month", target_disease = "WNV", pt_estimate = "mle",scale = 1000, "Cx pipiens", NULL))
  checkIR(getInfectionRate(sample_pools, interval = "Biweek", target_disease = "WNV", pt_estimate = "bc-mle",scale = 1000, c("Cx pipiens","Cx tarsalis"), c("GRVD","CO2")))
})

# Test cases for errors and warnings
test_that("Error and warning cases", {
  # Target year not in data

  # Insufficient pools data
  expect_error(getInfectionRate(sample_pools[1:3], interval = "Week",  target_disease = "WNV", pt_estimate = "mir" ), "Insufficent pools data")

  # Parameters are valid

  # Improper essential parameters throw an error
  expect_error(getInfectionRate(sample_pools, interval = "day",  target_disease = "WNV", pt_estimate = "bc-mle"), "Invalid parameters. See documentation for getInfectionRate()")
  expect_error(getInfectionRate(sample_pools, interval = "Week",  target_disease = "WN", pt_estimate = "mle"), "Invalid parameters. See documentation for getInfectionRate()")
  expect_error(getInfectionRate(sample_pools, interval = "Biweek",  target_disease = "WNV", pt_estimate = "mire"), "Invalid parameters. See documentation for getInfectionRate()")
  expect_error(getInfectionRate(sample_pools, interval = "Month",  target_disease = "SLEV", pt_estimate = ""), "Invalid parameters. See documentation for getInfectionRate()")



})


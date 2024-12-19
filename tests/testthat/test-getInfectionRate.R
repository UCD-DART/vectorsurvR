



# Helper function to check IR output is valid
checkIR <- function(IR) {
  expect_true(!is.null(IR))
  expect_s3_class(IR, "data.frame")
  expect_contains(colnames(IR), c("Year","Agency" ,"Disease","Species" ,"Trap","InfectionRate", "LowerCI", "UpperCI"))
}

# Test cases
test_that("Valid parameters returns a data frame formatted properly", {
  checkIR(getInfectionRate(testing_pools, interval = "Week", target_disease = "WNV", pt_estimate = "bc-mle",scale = 1000, NULL, NULL))
  checkIR(getInfectionRate(testing_pools, interval = "Biweek", target_disease = "WNV", pt_estimate = "mle",scale = 1000, list("Cx pipiens","Cx tarsalis"), NULL))
  checkIR(getInfectionRate(testing_pools, interval = "Month", target_disease = "WNV", pt_estimate = "mir",scale = 1000, NULL, c("GRVD")))
  checkIR(getInfectionRate(testing_pools, interval = "Month", target_disease = "WNV", pt_estimate = "mle",scale = 1000, "Cx pipiens", NULL,separate_by = "agency"))
  checkIR(getInfectionRate(testing_pools, interval = "Biweek", target_disease = "WNV", pt_estimate = "bc-mle",scale = 1000, c("Cx pipiens","Cx tarsalis"), c("GRVD","CO2"),separate_by=c("trap","species")))
})

# Test cases for errors and warnings
test_that("Error and warning cases", {
  # Target year not in data

  # Insufficient pools data
  expect_error(getInfectionRate(testing_pools[1:3], interval = "Week",  target_disease = "WNV", pt_estimate = "mir" ), "Insufficient pools data")

  # Parameters are valid

  # Improper essential parameters throw an error
  expect_error(getInfectionRate(testing_pools, interval = "day",  target_disease = "WNV", pt_estimate = "bc-mle"), "Invalid parameters. See documentation for getInfectionRate()")
  expect_error(getInfectionRate(testing_pools, interval = "Week",  target_disease = "WN", pt_estimate = "mle"), "Invalid parameters. See documentation for getInfectionRate()")
  expect_error(getInfectionRate(testing_pools, interval = "Biweek",  target_disease = "WNV", pt_estimate = "mire"), "Invalid parameters. See documentation for getInfectionRate()")
  expect_error(getInfectionRate(testing_pools, interval = "Month",  target_disease = "SLEV", pt_estimate = ""), "Invalid parameters. See documentation for getInfectionRate()")



})





checkAbundanceAB <- function(abundance) {
  expect_true(!is.null(abundance))
  expect_s3_class(abundance, "data.frame")
  expect_contains(colnames(abundance), list("Year","Agency", "Species","Count", "Trap","TrapEvents", "Abundance", "FiveYearAvg",  "YearsInAverage",  "Delta"))
}

#Test 1: Returns valid dataframe with valid paramaeters
test_that(" Returns valid dataframe with valid paramaeters",{
  checkAbundanceAB(getAbundanceAnomaly(testing_collections,interval = "Biweek", target_year =  2024))
  checkAbundanceAB(getAbundanceAnomaly(testing_collections,interval = "Week", target_year =  2024, sex = "male", trap="CO2"))
  checkAbundanceAB(getAbundanceAnomaly(testing_collections,interval = "Month", target_year =  2024, species = "Cx tarsalis" , separate_by = "agency"))
  checkAbundanceAB(getAbundanceAnomaly(testing_collections,interval = "Week", target_year =  2024, trap = "CO2"))
  checkAbundanceAB(getAbundanceAnomaly(testing_collections,interval = "Month", target_year =  2024, species =c("Cx tarsalis","Cx pipiens"),separate_by = c("agency", "trap")))

  })

#Test 2: Error Cases
test_that("Error thrown if target year missing",{

  expect_error(getAbundanceAnomaly(testing_collections,interval = "Biweek", target_year =  2025),"Target year not present in data.")
})



#Test 3: Warning
test_that("Warning if years before target year, check these are not included in calculation result",{

  expect_warning(getAbundanceAnomaly(sample_collections,interval = "Biweek", target_year =  2019),"There are years greater than the target year in the data. These years will not be included in the anomaly calculation.")
  #check that years before target year are
  #for(year in unique(ab$surv_year)){expect_lte(year,2021)}

})






# Test 4: Check if the function handles empty data gracefully
test_that("Handles wrong/incorrect collections data", {
  # Create an empty data frame
  empty_collections = data.frame()
  # Expect an error or specific behavior for empty data
  expect_error(getAbundanceAnomaly(empty_collections, interval = "Biweek", target_year = 2021), "Collections data is empty")
})
test_that("getAbundance returns accurate abundance", {
  # Assuming you have a specific test scenario
  true_species = "Cx pipiens, Cx tarsalis"
  true_abundance = 30.83
  true_avg = 25.9
  true_delta = 19.06

  # Now, you can compare the result of getAbundance with the manually calculated test_cut_aggregated
  # For example:
  result <- getAbundanceAnomaly(collections = testing_collections, target_year = 2024, interval = "Week", species = list("Cx pipiens","Cx tarsalis"), separate = "trap")
  # Assert that the result matches your expectations
  filter_res = result %>% filter(Year==2024, Trap=="CO2", Week==20)

  expect_equal(filter_res$Species, true_species)
  expect_equal(filter_res$Abundance, true_abundance)
  expect_equal(round(filter_res$FiveYearAvg,1), round(true_avg,1))
  expect_equal(round(filter_res$Delta), round(true_delta))

})
test_that("Error thrown when incorrect collections data",{

  expect_error(getAbundanceAnomaly(sample_collections[1:3]), "Insufficent collections data provided")

})


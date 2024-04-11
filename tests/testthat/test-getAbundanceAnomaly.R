


checkAbundanceAB <- function(abundance) {
  expect_true(!is.null(abundance))
  expect_s3_class(abundance, "data.frame")
  expect_contains(colnames(abundance)[2:8], list("surv_year", "Count", "Trap_Events", "Abundance", "Five_Year_Avg",  "Years_In_Average",  "Delta"))
}

#Test 1: Returns valid dataframe with valid paramaeters
test_that(" Returns valid dataframe with valid paramaeters",{  checkAbundanceAB(getAbundanceAnomaly(sample_collections,interval = "Biweek", target_year =  2020))
})

#Test 2: Error Cases
test_that("Error thorwn if target year missing",{

  expect_error(getAbundanceAnomaly(sample_collections,interval = "Biweek", target_year =  2024),"Target year not present in data.")
})



#Test 3: Warning
test_that("Warning if years before target year, check these are not included in calculation result",{

  expect_warning(getAbundanceAnomaly(sample_collections,interval = "Biweek", target_year =  2018, species_seperate=FALSE),"There are years greater than the target year in the data. These years will not be included in the anomaly calculation.")
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

test_that("Error thrown when incorrect collections data",{

  expect_error(getAbundanceAnomaly(sample_collections[1:3]), "Insufficent collections data provided")

})


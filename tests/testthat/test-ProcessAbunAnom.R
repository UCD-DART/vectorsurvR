
generateTestData <- function(startDate = as.Date("2016-01-01"), endDate = as.Date("2023-01-01"), chunkLength = 500) {
  chunk <- data.frame(
    collection_id = sample(1:500, chunkLength, replace = FALSE),
    collection_date = sample(seq(startDate, endDate, by = "days"), chunkLength),
    num_trap = sample(1:5, chunkLength, replace = TRUE),
    trap_nights = sample(1:10, chunkLength, replace = TRUE),
    trap_problem_bit = sample(c(TRUE, FALSE), chunkLength, replace = TRUE, prob = c(0.05, 0.95)),
    num_count = sample(0:50, chunkLength, replace = TRUE),
    sex_type = sample(c("male", "female"), chunkLength, replace = TRUE, prob = c(0.2, 0.8)),
    species_display_name = sample(c("Cx pipiens", "Cx tarsalis", "An freeborni"), chunkLength, replace = TRUE),
    trap_acronym = sample(c("CO2", "GRVD", "LCKR"), chunkLength, replace = TRUE)
  )

  chunk$surv_year = year(chunk$collection_date)
  return(chunk)
}

test=generateTestData()
ab = getAbundanceAnomaly(test, "Biweek", target_year = 2022,species_seperate = F)
processAbunAnom(ab)

# Test for ProcessAbunAnom function
test_that("processAbunAnom function processes the data correctly", {
  # Process the mocked AbAnomOutput
  intervals = c("Biweek", "Week", "Month")

  for (interval in intervals){
  ab = getAbundanceAnomaly(test, interval, target_year =2021)
  processed_output <- processAbunAnom(ab)
  # Check if the processed output has the correct structure
  expect_true(!FALSE %in% c(interval,"surv_year", "Count", "Trap_Events", "Years_In_Average", "Abundance_Type", "Abundance_Calculation" )%in% colnames(processed_output))



  }


})


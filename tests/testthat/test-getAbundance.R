
generateTestData <- function(startDate = as.Date("2020-01-01"), endDate = as.Date("2022-01-01"), chunkLength = 20) {
  chunk <- data.frame(
    collection_id = sample(1:100, chunkLength, replace = FALSE),
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

# Helper function to check abundance
checkAbundance <- function(abundance) {
  expect_true(!is.null(abundance))
  expect_s3_class(abundance, "data.frame")
  expect_contains(colnames(abundance), list("surv_year", "Count", "Trap_Events", "Abundance"))
}

# Generate simulated data for testing
#sample_collections <- generateTestData()

# Test valid data returns a data frame formatted properly
test_that("Valid data returns a data frame formatted properly", {
  checkAbundance(getAbundance(collections = sample_collections, interval = "Week", NULL, NULL, FALSE))
  checkAbundance(getAbundance(collections = sample_collections, interval = "Biweek", NULL, NULL, FALSE))
  checkAbundance(getAbundance(collections = sample_collections, interval = "Month", NULL, NULL, FALSE))
  checkAbundance(getAbundance(collections = sample_collections, interval = "Month", "Cx pipiens", NULL, FALSE))
  checkAbundance(getAbundance(collections = sample_collections, interval = "Biweek", NULL, "CO2", FALSE))
})

# Test getAbundance returns accurate abundance
test_that("getAbundance returns accurate abundance", {
  # Assuming you have a specific test scenario
  test_cut <- sample_collections[!sample_collections$trap_problem_bit & sample_collections$sex_type=="female", ]
  test_cut$Week <- epiweek(test_cut$collection_date)
  test_cut_aggregated <- test_cut %>% group_by(surv_year, Week) %>% mutate(TN = (trap_nights * num_trap)) %>% summarize(Trap_Events = sum(TN))
  test_cut_count = test_cut %>% group_by(surv_year, Week)%>%dplyr::filter(species_display_name =="Cx pipiens") %>% summarize(Count = sum(num_count))

  # Now, you can compare the result of getAbundance with the manually calculated test_cut_aggregated
  # For example:
  result <- getAbundance(collections = sample_collections, interval = "Week", species_list = list("Cx pipiens"), species_seperate = FALSE)
  # Assert that the result matches your expectations
  expect_equal(sort(test_cut_count$Count, decreasing = T), sort(result$Count, decreasing = T))
})

test_that("Error thrown when incorrect collections data",{

expect_error(getAbundance(sample_collections[c(1:3)]), "Insufficent collections data provided")
  })


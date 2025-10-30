test_that("plotVectorAbundance handles edge cases", {
  mockery::stub(plotVectorAbundance, "getArthroCollections", vectorsurvR:::testing_collections)

  test_data <- vectorsurvR:::testing_collections
  valid_species <- unique(test_data$species_display_name)[1]
  valid_trap <- unique(test_data$trap_acronym)[1]

  # Test when target_year not in data (should error)
  # This depends on your testing data - you might need to create a specific case
  expect_error(
    plotVectorAbundance(
      token = "dummy-token",
      interval = "Week",
      target_year = 1900,  # Year that doesn't exist in testing data
      species = valid_species,
      trap = valid_trap
    ),
    "Target year not in data"
  )
})

test_that("plotVectorAbundance works with different sex parameters", {
  mockery::stub(plotVectorAbundance, "getArthroCollections", vectorsurvR:::testing_collections)

  test_data <- vectorsurvR:::testing_collections
  valid_species <- unique(test_data$species_display_name)[2]
  valid_trap <- unique(test_data$trap_acronym)[1]

  # Test with different sex parameter
  result <- plotVectorAbundance(
    token = "dummy-token",
    interval = "Week",
    target_year = 2024,
    species = valid_species,
    trap = valid_trap,
    sex = "female"  # Explicitly set
  )

  expect_s3_class(result, "ggplot")
})


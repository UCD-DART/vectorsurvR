library(mockery)

# Helper to get testing token (if needed for function signature)
get_testing_token <- function() {
  # Return a dummy token - it won't be used since we'll bypass getArthroCollections
  "testing-token-123"
}

# Helper to get testing collections directly
get_testing_collections <- function() {
  vectorsurvR:::testing_collections
}

# Add to DESCRIPTION imports or suggests
# Imports: mockery

test_that("getSpeciesTable works with testing data", {
  # Mock the specific function call inside getSpeciesTable
  mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

  result <- getSpeciesTable(
    token = "dummy-token",
    interval = "Week",
    target_year = 2023,
    output_format = "simple"
  )

  expect_s3_class(result, "data.frame")
  expect_true("Week" %in% names(result))
  expect_true("Species" %in% names(result))
  expect_true("Count" %in% names(result))
})
test_that("getSpeciesTable handles cumulative calculations with testing data", {

    mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

      result <- getSpeciesTable(
        token = "dummy-token",
        interval = "Month",
        target_year = 2023,
        cumulative = TRUE,
        output_format = "simple"
      )

      expect_true("Cumulative_Count" %in% names(result))


})

test_that("getSpeciesTable handles grouping with testing data", {
  mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

      # Test single grouping
      result_city <- getSpeciesTable(
        token = "dummy-token",
        interval = "Month",
        target_year = 2023,
        separate_by = "city",
        output_format = "simple"
      )
      expect_true("City" %in% names(result_city))

      # Test multiple groupings
      result_multi <- getSpeciesTable(
        token = "dummy-token",
        interval = "Month",
        target_year = 2023,
        separate_by = c("city", "county"),
        output_format = "simple"
      )
      expect_true(all(c("City", "County") %in% names(result_multi)))


})

test_that("getSpeciesTable handles filtering parameters with testing data", {
  mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

      # Get unique species from testing data to use realistic filters
      test_species <- unique(vectorsurvR:::testing_collections$species_display_name)
      test_traps <- unique(vectorsurvR:::testing_collections$trap_acronym)

      # Test species filtering
      if (length(test_species) > 0) {
        result_species <- getSpeciesTable(
          token = "dummy-token",
          interval = "Month",
          target_year = 2023,
          species = test_species[1],
          output_format = "simple"
        )
        expect_true(nrow(result_species) > 0)
      }

      # Test trap filtering
      if (length(test_traps) > 0) {
        result_trap <- getSpeciesTable(
          token = "dummy-token",
          interval = "Month",
          target_year = 2023,
          trap = test_traps[1],
          output_format = "simple"
        )
        expect_true(nrow(result_trap) > 0)
      }


})

test_that("getSpeciesTable output formats work with testing data", {
  mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

      # Test simple output
      result_simple <- getSpeciesTable(
        token = "dummy-token",
        interval = "Month",
        target_year = 2023,
        output_format = "simple"
      )
      expect_s3_class(result_simple, "data.frame")

      # Test HTML output
      result_html <- getSpeciesTable(
        token = "dummy-token",
        interval = "Month",
        target_year = 2023,
        output_format = "html"
      )
      expect_true(inherits(result_html, "knitr_kable"))


})

test_that("getSpeciesTable parameter validation works", {
  mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

      # Test invalid grouping
      expect_error(
        getSpeciesTable(
          token = "dummy-token",
          interval = "Month",
          target_year = 2023,
          separate_by = "invalid_group"
        ),
        "Invalid grouping"
      )

})

test_that("getSpeciesTable handles edge cases with testing data", {
  mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

      # Test with include_trap_nights = FALSE
      result_no_trap <- getSpeciesTable(
        token = "dummy-token",
        interval = "Month",
        target_year = 2023,
        include_trap_nights = FALSE,
        output_format = "simple"
      )
      expect_false("TrapEvents" %in% names(result_no_trap))

      # Test with include_abundance = TRUE (non-cumulative)
      result_abund <- getSpeciesTable(
        token = "dummy-token",
        interval = "Month",
        target_year = 2023,
        include_abundance = TRUE,
        cumulative = FALSE,
        output_format = "simple"
      )
      expect_true("Abundance" %in% names(result_abund))

})

test_that("getSpeciesTable works with different intervals", {
  mockery::stub(getSpeciesTable, "getArthroCollections", vectorsurvR:::testing_collections)

      intervals <- c("CollectionDate", "Week", "Biweek", "Month")

      for (int in intervals) {
        result <- getSpeciesTable(
          token = "dummy-token",
          interval = int,
          target_year = 2023,
          output_format = "simple"
        )
        expect_s3_class(result, "data.frame")
        expect_true(int %in% names(result))
      }

})

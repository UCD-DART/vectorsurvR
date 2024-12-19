

# Helper function to check abundance
checkAbundance <- function(abundance) {
  expect_true(!is.null(abundance))
  expect_s3_class(abundance, "data.frame")
  expect_contains(colnames(abundance), list("Agency","Year", "Count","Species", "Trap","TrapEvents", "Abundance"))
}

# Generate simulated data for testing

# Test valid data returns a data frame formatted properly
test_that("Valid data returns a data frame formatted properly", {
  checkAbundance(getAbundance(collections = testing_collections, interval = "Week", NULL, NULL, ))
  checkAbundance(getAbundance(collections = testing_collections, interval = "Biweek", NULL, NULL, ))
  checkAbundance(getAbundance(collections = testing_collections, interval = "Month", NULL, NULL, ))
  checkAbundance(getAbundance(collections = testing_collections, interval = "Month", "Cx pipiens", NULL, NULL,separate_by = "agency"))
  checkAbundance(getAbundance(collections = testing_collections, interval = "Biweek", NULL, c("CO2","GRVD"), NULL))
})


# Test getAbundance returns accurate abundance
test_that("getAbundance returns accurate abundance", {
  # Assuming you have a specific test scenario
  true_species = "Cx pipiens, Cx tarsalis"
  true_abundance = 19.09

  # Now, you can compare the result of getAbundance with the manually calculated test_cut_aggregated
  # For example:
  result <- getAbundance(collections = testing_collections, interval = "Week", species = list("Cx pipiens","Cx tarsalis"), separate = "trap")
  # Assert that the result matches your expectations
  filter_res = result %>% filter(Year==2020, Trap=="CO2", Week==20)

   expect_equal(filter_res$Species, true_species)
   expect_equal(filter_res$Abundance, true_abundance)

})


test_that("Seperate_by works", {
  AB = (getAbundance(collections = testing_collections, interval = "Month", c("Cx pipiens", "Cx tarsalis"), c("CO2","GRVD"), NULL, separate_by = c("agency", "species", "trap")))
  checkAbundance(getAbundance(collections = testing_collections, interval = "Month", c("Cx pipiens", "Cx tarsalis"), c("CO2","GRVD"), NULL, separate_by = c("agency", "species", "trap")))
  expect_contains(colnames(AB), "Month")
  expect_setequal(unique(AB$Agency), c("SAYO","SLCM"))
  expect_setequal(unique(AB$Species), c("Cx pipiens", "Cx tarsalis"))
  expect_setequal(unique(AB$Trap), c("CO2","GRVD"))


})

test_that("Seperate_by works when no separating", {
  AB = (getAbundance(collections = testing_collections, interval = "Week", c("Cx pipiens", "Cx tarsalis"), c("CO2","GRVD"), "female"))
  checkAbundance(getAbundance(collections = testing_collections, interval = "Week", c("Cx pipiens", "Cx tarsalis"), c("CO2","GRVD"), "female"))
  expect_contains(colnames(AB), "Week")
  expect_setequal(sort(unique(AB$Agency)), c("SAYO","SAYO, SLCM", "SLCM"))
  expect_setequal(unique(AB$Species), c("Cx pipiens", "Cx pipiens, Cx tarsalis"))
  expect_setequal(unique(AB$Trap), c("CO2","CO2, GRVD","GRVD"))


})

test_that("Error thrown when incorrect collections data",{

expect_error(getAbundance(testing_collections[,c(1:3)]), "Insufficient collections data provided")})


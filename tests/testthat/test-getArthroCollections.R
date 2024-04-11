## Helper function to check collections
#checkCollections <- function(collections) {
#  expect_true(!is.null(collections))
#  expect_s3_class(collections, "data.frame")
#  expect_contains(colnames(collections), list("collection_id", "collection_num", "collection_date", "collection_date_date_only", "comments",
#                                              "identified_by", "num_trap", "site", "surv_year", "trap_nights", "trap_problem_bit",
#                                              "user", "add_date", "deactive_date", "updated", "id", "num_count", "sex_id", "sex_type",
#                                              "sex_name", "species_id", "species_full_name", "species_display_name", "agency_id",
#                                              "agency_code", "agency_name", "trap_id", "trap_acronym", "trap_name", "trap_presence"))
#
#  expect_contains(unique(collections$surv_year), list(2020, 2021))
#}
## Assuming you have a valid token
#valid_token <- getToken()
#
## Test 1: Successful data retrieval
#test_that("Successful data retrieval with valid token", {
#  collections <- getArthroCollections(valid_token, 2020, 2021)
#  checkCollections(collections)
#})
#
#
## Test 3: Unauthorized access (simulating invalid token)
#test_that("Null token from wrong credentials)", {
#
#  expect_error(getArthroCollections(NULL, 2020, 2021), "Invalid token. Check username and password")
#})
#
## Test 4: Impossible year range
#test_that("Impossible year range", {
#  expect_error(getArthroCollections(valid_token, 2020, 3000), "Impossible year range. Check end_year")
#})
#
## Test 5: Invalid date range (start_year > end_year)
#test_that("Invalid date range (start_year > end_year)", {
#  expect_error(getArthroCollections(valid_token, 2022, 2021), "Invalid year range, check parameters")
#})
#
## Test 6: Invalid date format (start_year and end_year not numeric)
#test_that("Invalid date format (start_year and end_year not numeric)", {
#  expect_error(getArthroCollections(valid_token, "start", "end"), "Incorrect date format, start_year and end_year must be numeric")
#})
#

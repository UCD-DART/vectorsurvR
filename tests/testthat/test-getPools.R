#checkPools<- function(pools) {
#  expect_true(!is.null(pools))
#  expect_s3_class(pools, "data.frame")
#  expect_contains(colnames(pools), list("pool_id",
#                                        "pool_num",
#                                        "primary_source",
#                                        "collection",
#                                        "comments",
#                                        "surv_year",
#                                        "site",
#                                        "collection_date",
#                                        "sex",
#                                        "num_count",
#                                        "add_date",
#                                        "updated",
#                                        "id",
#                                        "value",
#                                        "test_date",
#                                        "test_comments",
#                                        "method_id",
#                                        "pool_comments",
#                                        "method_acronym",
#                                        "method_sensitivity",
#                                        "target_id",
#                                        "target_name",
#                                        "target_description",
#                                        "target_acronym",
#                                        "target_vector",
#                                        "target_icd_10",
#                                        "status_id",
#                                        "status_name",
#                                        "status_description",
#                                        "test_agency_id",
#                                        "test_agency_name",
#                                        "test_agency_code",
#                                        "test_agency_region",
#                                        "test_agency_state_id",
#                                        "test_agency_state_acronym",
#                                        "test_agency_state_name", "trap_id",
#                                        "trap_acronym",
#                                        "trap_name",
#                                        "trap_presence",
#                                        "species_id",
#                                        "species_full_name",
#                                        "species_display_name",
#                                        "agency_id",
#                                        "agency_code",
#                                        "agency_name"))
#
#  expect_contains(unique(pools$surv_year), list(2020, 2021))
#}
#
#
#
## Assuming you have a valid token
#valid_token <- getToken()
#
## Test 1: Successful data retrieval
#test_that("Successful data retrieval with valid token", {
#  pools <- getPools(valid_token, 2020, 2021)
#  checkPools(pools)
#})
#
#
## Test 3: Unauthorized access (simulating invalid token)
#test_that("Null token from wrong credentials)", {
#
#  expect_error(getPools(NULL, 2020, 2021), "Invalid token. Check username and password")
#})
#
## Test 4: Impossible year range
#test_that("Impossible year range", {
#  expect_error(getPools(valid_token, 2020, 3000), "Impossible year range. Check end_year")
#})
#
## Test 5: Invalid date range (start_year > end_year)
#test_that("Invalid date range (start_year > end_year)", {
#  expect_error(getPools(valid_token, 2022, 2021), "Invalid year range, check parameters")
#})
#
## Test 6: Invalid date format (start_year and end_year not numeric)
#test_that("Invalid date format (start_year and end_year not numeric)", {
#  expect_error(getPools(valid_token, "start", "end"), "Incorrect date format, start_year and end_year must be numeric")
#})
#
#

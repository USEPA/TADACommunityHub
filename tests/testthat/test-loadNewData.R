# Test that validateATTAINSParam function correctly identify data validation errors, if any.
test_that("Does the current valiadateATTAINSParam identify all non-valid ATTAINS parameter name?", {
  # Check for any new domain values for ATTAINS Parameters
  validate.test <- validateATTAINSParam(UTAHDWQ)

  # Uncomment the two lines below if you need to update the test data
  # ATTAINS_param_name <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))
  # saveRDS(ATTAINS_param_name, file = "tests/testthat/data/ATTAINS_param_name.rds")  
  
  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS_param_name <- readRDS(system.file("extdata", "ATTAINS_param_name.rds", package = "TADACommunityHub"))

  # Validate_test should not contain any param values in ATTAINS.raw
  validate.test.param <- validate.test$issues
  ATTAINS.raw.param <- ATTAINS_param_name$code

  expect_disjoint(validate.test.param, ATTAINS.raw.param)
})

# Test that validateATTAINSUse functions correctly identify data validation errors, if any.
test_that("Does the current valiadateATTAINSUse identify all non-valid ATTAINS use name?", {
  # Check for any new domain values for ATTAINS Uses
  validate.test <- validateATTAINSUse(UTAHDWQ)

  # Uncomment the two lines below if you need to update the test data
  # ATTAINS_use_name <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("use_name"))
  # saveRDS(ATTAINS_use_name, file = "tests/testthat/data/ATTAINS_use_name.rds")
  
  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS_use_name <- readRDS(system.file("extdata", "ATTAINS_use_name.rds", package = "TADACommunityHub"))
  
  # Validate_test should not contain any use values in ATTAINS.raw
  validate.test.use <- validate.test$issues
  ATTAINS.raw.use <- ATTAINS_use_name$code

  expect_disjoint(validate.test.use, ATTAINS.raw.use)
})

# Test that validateATTAINSOrg functions correctly identify data validation errors, if any.
test_that("Does the current validateATTAINSOrg identify all non-valid ATTAINS org id?", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  
  # Check for any new domain values for ATTAINS org_id
  validate.test <- validateATTAINSUse(UTAHDWQ)

  # Uncomment the two lines below if you need to update the test data
  # ATTAINS_org_id <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))
  # saveRDS(ATTAINS_org_id, file = "tests/testthat/data/ATTAINS_org_id.rds")
  
  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS_org_id <- readRDS(system.file("extdata", "ATTAINS_org_id.rds", package = "TADACommunityHub"))
  
  # Validate_test should not contain any use values in ATTAINS.raw
  validate.test.org <- validate.test$issues
  ATTAINS.raw.org <- ATTAINS_org_id$code

  expect_disjoint(validate.test.org, ATTAINS.raw.org)
})

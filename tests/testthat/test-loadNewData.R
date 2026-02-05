# Test that validateATTAINSParam function correctly identify data validation errors, if any.
test_that("Does the current valiadateATTAINSParam identify all non-valid ATTAINS parameter name?", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  
  # Check for any new domain values for ATTAINS Parameters
  validate.test <- validateATTAINSParam(UTAHDWQ)

  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))

  # Validate_test should not contain any param values in ATTAINS.raw
  validate.test.param <- validate.test$issues
  ATTAINS.raw.param <- ATTAINS.raw$code

  expect_disjoint(validate.test.param, ATTAINS.raw.param)
})

# Test that validateATTAINSUse functions correctly identify data validation errors, if any.
test_that("Does the current valiadateATTAINSUse identify all non-valid ATTAINS use name?", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  
  # Check for any new domain values for ATTAINS Uses
  validate.test <- validateATTAINSUse(UTAHDWQ)

  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("use_name"))

  # Validate_test should not contain any use values in ATTAINS.raw
  validate.test.use <- validate.test$issues
  ATTAINS.raw.use <- ATTAINS.raw$code

  expect_disjoint(validate.test.use, ATTAINS.raw.use)
})

# Test that validateATTAINSOrg functions correctly identify data validation errors, if any.
test_that("Does the current validateATTAINSOrg identify all non-valid ATTAINS org id?", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  
  # Check for any new domain values for ATTAINS org_id
  validate.test <- validateATTAINSUse(UTAHDWQ)

  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))

  # Validate_test should not contain any use values in ATTAINS.raw
  validate.test.org <- validate.test$issues
  ATTAINS.raw.org <- ATTAINS.raw$code

  expect_disjoint(validate.test.org, ATTAINS.raw.org)
})

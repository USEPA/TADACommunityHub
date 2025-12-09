# Test that validate functions correctly identify data validation errors, if any.
test_that("Does the current TADA_GetATTAINSParamToWQPCharRef contain all ATTAINS parameter name?", {
  # Check for any new domain values for ATTAINS Parameters
  data("UTAHDWQ")
  validate.test <- validateATTAINSParam(UTAHDWQ)
  
  # Retrieve the ATTAINS domain value from rExpertQuery
  ATTAINS.raw <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))
  
  # Validate_test should not contain any param values in ATTAINS.raw
  validate.test.param <- validate.test$issues
  ATTAINS.raw.param <- utils::read.csv(system.file(
    "extdata",
    "ATTAINSParamToWQPCharRef.csv",
    package = "EPATADA"
  ))[, "ATTAINS.ParameterName"]
  
  expect_disjoint(validate.test.param, ATTAINS.raw.param)
})

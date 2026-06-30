# -----------------------------
# ATTAINS Parameter Name
# -----------------------------
test_that("validateATTAINSParam identifies all non-valid ATTAINS parameter names", {
  skip_on_cran()
  testthat::skip_if_offline()

  # Run validation
  res <- validateATTAINSParam(UTAHDWQ)

  # Retrieve the ATTAINS domain values for parameters
  domain_df <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))
  # Note: validateATTAINSParam uses 'name' column from EQ_DomainValues
  domain_vals <- toupper(as.character(domain_df$name))

  # Issues should contain only invalid entries (thus disjoint with domain)
  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# ATTAINS Use Name
# -----------------------------
test_that("validateATTAINSUse identifies all non-valid ATTAINS use names", {
  skip_on_cran()
  testthat::skip_if_offline()

  # Run validation
  res <- validateATTAINSUse(UTAHDWQ)

  # Retrieve the ATTAINS domain values for uses (uses 'code')
  domain_df <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("use_name"))
  domain_vals <- toupper(as.character(domain_df$code))

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# ATTAINS Organization Identifier
# -----------------------------
test_that("validateATTAINSOrg identifies all non-valid ATTAINS organization identifiers", {
  skip_on_cran()
  testthat::skip_if_offline()

  # Run validation
  res <- validateATTAINSOrg(UTAHDWQ)

  # Retrieve the ATTAINS domain values for org_id (uses 'code')
  domain_df <- spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id"))
  domain_vals <- toupper(as.character(domain_df$code))

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# WQX Characteristic Name
# -----------------------------
test_that("validateWQXChar identifies all non-valid WQX characteristic names", {
  skip_on_cran()
  testthat::skip_if_offline()

  # Run validation
  res <- validateWQXChar(UTAHDWQ)

  # Retrieve WQX domain values (Characteristic CSV: 'Name' column)
  domain_df <- utils::read.csv(
    url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"),
    stringsAsFactors = FALSE
  )
  expect_true("Name" %in% names(domain_df))
  domain_vals <- toupper(as.character(domain_df$Name))
  domain_vals <- domain_vals[!is.na(domain_vals) & nzchar(domain_vals)]

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# WQX Measure Unit (MagnitudeUnit)
# -----------------------------
test_that("validateWQXUnits identifies all non-valid WQX measure units (MagnitudeUnit)", {
  skip_on_cran()
  testthat::skip_if_offline()

  # Run validation
  res <- validateWQXUnits(UTAHDWQ)

  # Retrieve WQX domain values (MeasureUnit CSV: 'Target.Unit' column)
  domain_df <- utils::read.csv(
    url("https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"),
    stringsAsFactors = FALSE
  )
  expect_true("Target.Unit" %in% names(domain_df))
  domain_vals <- toupper(as.character(domain_df$Target.Unit))
  domain_vals <- domain_vals[!is.na(domain_vals) & nzchar(domain_vals)]

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# Duration Units
# -----------------------------
test_that("validateDurationUnits identifies all non-valid DurationUnit values", {
  # Run validation (no network calls)
  res <- validateDurationUnits(UTAHDWQ)

  # Allowed domain for DurationUnit per implementation
  domain_vals <- toupper(c("n-hour", "n-day", "n-week", "n-month", "n-quarter"))

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# Frequency Methods
# -----------------------------
test_that("validateFreqMethod identifies all non-valid FreqMethod values", {
  # Run validation (no network calls)
  res <- validateFreqMethod(UTAHDWQ)

  # Allowed domain for FreqMethod per implementation
  domain_vals <- toupper(c(
    "Percent of samples not meeting",
    "percentile",
    "n-samples in 3 years",
    "n-samples in 4 years",
    "n-samples in 5 years",
    "binomial test",
    "NumberNotMeeting"
  ))

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# Duration Methods
# -----------------------------
test_that("validateDurationMethod identifies all non-valid DurationMethod values", {
  # Run validation (no network calls)
  res <- validateDurationMethod(UTAHDWQ)

  # Allowed domain for DurationMethod per implementation
  domain_vals <- toupper(c(
    "arithmetic mean",
    "arithmetic median",
    "arithmetic max",
    "arithmetic min",
    "arithmetic extremes",
    "geometric mean",
    "rolling geometric mean",
    "rolling arithmetic mean",
    "mean of daily minima",
    "mean of daily maxima"
  ))

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# Season
# -----------------------------
test_that("validateSeason identifies all non-valid Season values", {
  # Run validation (no network calls)
  res <- validateSeason(UTAHDWQ)

  # Allowed domain for Season per implementation
  domain_vals <- toupper(c("Summer", "Fall", "Spring", "Winter"))

  expect_true(is.list(res))
  expect_true("issues" %in% names(res))
  expect_disjoint(toupper(res$issues), domain_vals)
})

# -----------------------------
# Run all validations and check structure
# -----------------------------
test_that("runAllValidations returns expected structure and summary", {
  # This runs all validators; some may be Rejected/Error depending on data
  res <- runAllValidations(UTAHDWQ)

  expect_true(is.list(res))
  expect_true(all(c("overall_status", "summary", "results") %in% names(res)))

  # Check summary structure
  expect_true(is.data.frame(res$summary))
  expect_true(all(
    c(
      "validator",
      "status",
      "nrows_passes",
      "nrows_fails",
      "nrows_na",
      "issues_count"
    ) %in%
      names(res$summary)
  ))

  # Results list should be aligned to summary validators
  expect_equal(length(res$results), nrow(res$summary))
  expect_setequal(names(res$results), res$summary$validator)
})

# -----------------------------
# Validate all files helper
# -----------------------------
test_that("validateAllFiles returns a list of results for provided folder", {
  skip_on_cran()

  # Use default extdata folder if present; may be empty in some environments
  # This call should not error
  out <- validateAllFiles(validateColumn = validateSeason)

  expect_true(is.list(out))
  # Contents may be NULL for some files due to safe wrapper; do not assert more strictly
})

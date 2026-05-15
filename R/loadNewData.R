#' Load User Data - Validate ATTAINS.ParameterName
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all ATTAINS.ParameterName are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateATTAINSParam(UTAHDWQ)
#'
validateATTAINSParam <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"ATTAINS.ParameterName" %in% names(submitted_data)) {
    stop("Required column 'ATTAINS.ParameterName' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$ATTAINS.ParameterName <- as.character(submitted_data$ATTAINS.ParameterName)
  
  # Get domain values (per EQ_DomainValues message: use the 'name' column)
  domain_df <- tryCatch(
    {
      # Suppress messages from the web service call
      spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name"))
    },
    error = function(e) stop("Could not retrieve domain values: ", conditionMessage(e))
  )
  
  # Extract domain column
  domain_codes <- toupper(as.character(domain_df[["name"]]))
  
  # Build validation rule
  rules_values <- validate::validator(
    toupper(ATTAINS.ParameterName) %in% domain_codes
  )
  
  # Confront data with rules, passing domain_codes as a reference environment
  # This avoids evaluation errors where 'domain_codes' is not found.
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail counts directly (robust even if validate reports an error)
  in_domain <- toupper(submitted_data[["ATTAINS.ParameterName"]]) %in% domain_codes
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na <- report$nNA
  
  # Determine acceptance: reject if any fail OR if validate reported an error
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (including NA)
  issues <- unique(stats::na.omit(submitted_data$ATTAINS.ParameterName[is.na(in_domain)]))
  
  # Build result
  result <- list(
    status = status,
    message = if (accepted) {
      "ATTAINS.ParameterName(s) passed all validation checks."
    } else {
      "ATTAINS.ParameterName(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
  
  return(result)
}



#' Load User Data - Validate WQX Characteristic Names
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all WQX Characteristic Names are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateWQXChar(UTAHDWQ)
#'
# Validate WQX Characteristic Names against WQX domain list
validateWQXChar <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"TADA.CharacteristicName" %in% names(submitted_data)) {
    stop("Required column 'TADA.CharacteristicName' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$TADA.CharacteristicName <- as.character(submitted_data$TADA.CharacteristicName)
  
  # Get domain values from WQX download
  domain_df <- tryCatch(
    utils::read.csv(
      url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"),
      stringsAsFactors = FALSE
    ),
    error = function(e) stop("Could not retrieve WQX Characteristic domain values: ", conditionMessage(e))
  )
  
  if (!"Name" %in% names(domain_df)) {
    stop("WQX Characteristic domain file does not contain column 'Name'.")
  }
  
  domain_codes <- unique(toupper(as.character(domain_df[["Name"]])))
  domain_codes <- domain_codes[!is.na(domain_codes) & nzchar(domain_codes)]
  if (length(domain_codes) == 0L) {
    stop("Retrieved WQX Characteristic domain values are empty; cannot validate.")
  }
  
  # Build validation rule
  rules_values <- validate::validator(
    toupper(TADA.CharacteristicName) %in% domain_codes
  )
  
  # Confront data with rules, passing domain_codes as reference
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail counts directly
  in_domain <- toupper(submitted_data[["TADA.CharacteristicName"]]) %in% domain_codes
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$TADA.CharacteristicName[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "WQX.CharacteristicName(s) passed all validation checks."
    } else {
      "WQX.CharacteristicName(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}



#' Load User Data - Validate ATTAINS Use Names
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all ATTAINS Use Names are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' \dontrun{
#' validateATTAINSUse(UTAHDWQ)
#' }
#'
# Validate ATTAINS Use Names (final format)
validateATTAINSUse <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"ATTAINS.UseName" %in% names(submitted_data)) {
    stop("Required column 'ATTAINS.UseName' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$ATTAINS.UseName <- as.character(submitted_data$ATTAINS.UseName)
  
  # Get domain values
  domain_df <- tryCatch(
    spsUtil::quiet(rExpertQuery::EQ_DomainValues("use_name")),
    error = function(e) stop("Could not retrieve ATTAINS Use domain values: ", conditionMessage(e))
  )
  
  if (!"code" %in% names(domain_df)) {
    stop("ATTAINS Use domain values do not contain column 'code'.")
  }
  
  domain_codes <- unique(toupper(as.character(domain_df[["code"]])))
  domain_codes <- domain_codes[!is.na(domain_codes) & nzchar(domain_codes)]
  if (length(domain_codes) == 0L) {
    stop("Retrieved ATTAINS Use domain values are empty; cannot validate.")
  }
  
  # Build validation rule
  rules_values <- validate::validator(
    toupper(ATTAINS.UseName) %in% domain_codes
  )
  
  # Confront data with rules
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail/NA counts from report
  in_domain <- toupper(submitted_data[["ATTAINS.UseName"]]) %in% domain_codes
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na     <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$ATTAINS.UseName[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "ATTAINS.UseName(s) passed all validation checks."
    } else {
      "ATTAINS.UseName(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}


# Validate ATTAINS Organization Identifiers (final format)
validateATTAINSOrg <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"ATTAINS.OrganizationIdentifier" %in% names(submitted_data)) {
    stop("Required column 'ATTAINS.OrganizationIdentifier' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$ATTAINS.OrganizationIdentifier <- as.character(submitted_data$ATTAINS.OrganizationIdentifier)
  
  # Get domain values
  domain_df <- tryCatch(
    spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id")),
    error = function(e) stop("Could not retrieve ATTAINS Organization domain values: ", conditionMessage(e))
  )
  
  if (!"code" %in% names(domain_df)) {
    stop("ATTAINS Organization domain values do not contain column 'code'.")
  }
  
  domain_codes <- unique(toupper(as.character(domain_df[["code"]])))
  domain_codes <- domain_codes[!is.na(domain_codes) & nzchar(domain_codes)]
  if (length(domain_codes) == 0L) {
    stop("Retrieved ATTAINS Organization domain values are empty; cannot validate.")
  }
  
  # Build validation rule
  rules_values <- validate::validator(
    toupper(ATTAINS.OrganizationIdentifier) %in% domain_codes
  )
  
  # Confront data with rules
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail/NA counts from report
  in_domain <- toupper(submitted_data[["ATTAINS.OrganizationIdentifier"]]) %in% domain_codes
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na     <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$ATTAINS.OrganizationIdentifier[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "ATTAINS.OrganizationIdentifier(s) passed all validation checks."
    } else {
      "ATTAINS.OrganizationIdentifier(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}

#' Load User Data - Validate TADA Magnitude Units
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all Magnitude units are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateWQXUnits(UTAHDWQ)
#'
# Validate WQX Measure Units for Magnitude (final format; NA not allowed)
validateWQXUnits <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"MagnitudeUnit" %in% names(submitted_data)) {
    stop("Required column 'MagnitudeUnit' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$MagnitudeUnit <- as.character(submitted_data$MagnitudeUnit)
  
  # Get domain values
  domain_df <- tryCatch(
    utils::read.csv(
      url("https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV"),
      stringsAsFactors = FALSE
    ),
    error = function(e) stop("Could not retrieve WQX MeasureUnit domain values: ", conditionMessage(e))
  )
  
  if (!"Target.Unit" %in% names(domain_df)) {
    stop("WQX MeasureUnit domain file does not contain column 'Target.Unit'.")
  }
  
  domain_codes <- unique(toupper(as.character(domain_df[["Target.Unit"]])))
  domain_codes <- domain_codes[!is.na(domain_codes) & nzchar(domain_codes)]
  if (length(domain_codes) == 0L) {
    stop("Retrieved WQX MeasureUnit domain values are empty; cannot validate.")
  }
  
  # Build validation rule (NA not allowed)
  rules_values <- validate::validator(
    !is.na(MagnitudeUnit) & toupper(MagnitudeUnit) %in% domain_codes
  )
  
  # Confront data with rules
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail/NA counts from report
  in_domain <- !is.na(submitted_data[["MagnitudeUnit"]]) &
    (toupper(submitted_data[["MagnitudeUnit"]]) %in% domain_codes)
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na     <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$MagnitudeUnit[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "MagnitudeUnit(s) passed all validation checks."
    } else {
      "MagnitudeUnit(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}



#' Load User Data - Validate Duration Units
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all Duration units are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateDurationUnits(UTAHDWQ)
#'
# Validate Duration Units (final format; NA allowed as pass)
validateDurationUnits <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"DurationUnit" %in% names(submitted_data)) {
    stop("Required column 'DurationUnit' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$DurationUnit <- as.character(submitted_data$DurationUnit)
  
  # Allowed values
  domain <- c("n-hour", "n-day", "n-week", "n-month", "n-quarter")
  domain_codes <- toupper(domain)
  
  # Build validation rule (NA allowed)
  rules_values <- validate::validator(
    is.na(DurationUnit) | toupper(DurationUnit) %in% domain_codes
  )
  
  # Confront data with rules
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail/NA counts from report
  in_domain <- is.na(submitted_data[["DurationUnit"]]) |
    (toupper(submitted_data[["DurationUnit"]]) %in% domain_codes)
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na     <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$DurationUnit[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "DurationUnit(s) passed all validation checks."
    } else {
      "DurationUnit(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}




#' Load User Data - Validate Frequency Methods
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all frequency methods are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' data("UTAHDWQ")
#' validateFreqMethod(UTAHDWQ)
#'
# Validate Frequency Methods (final format; NA allowed as pass)
validateFreqMethod <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"FreqMethod" %in% names(submitted_data)) {
    stop("Required column 'FreqMethod' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$FreqMethod <- as.character(submitted_data$FreqMethod)
  
  # Allowed values
  domain <- c(
    "Percent of samples not meeting",
    "percentile",
    "n-samples in 3 years",
    "n-samples in 4 years",
    "n-samples in 5 years",
    "binomial test",
    "NumberNotMeeting"
  )
  domain_codes <- toupper(domain)
  
  # Build validation rule (NA allowed)
  rules_values <- validate::validator(
    is.na(FreqMethod) | toupper(FreqMethod) %in% domain_codes
  )
  
  # Confront data with rules
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail/NA counts from report
  in_domain <- is.na(submitted_data[["FreqMethod"]]) |
    (toupper(submitted_data[["FreqMethod"]]) %in% domain_codes)
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na     <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$FreqMethod[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "FreqMethod(s) passed all validation checks."
    } else {
      "FreqMethod(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}



#' Load User Data - Validate Duration Methods
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all Duration Methods are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' validateDurationMethod(UTAHDWQ)
#'
# Validate Duration Methods (final format; NA allowed as pass)
validateDurationMethod <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"DurationMethod" %in% names(submitted_data)) {
    stop("Required column 'DurationMethod' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$DurationMethod <- as.character(submitted_data$DurationMethod)
  
  # Allowed values
  domain <- c(
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
  )
  domain_codes <- toupper(domain)
  
  # Build validation rule (NA allowed)
  rules_values <- validate::validator(
    is.na(DurationMethod) | toupper(DurationMethod) %in% domain_codes
  )
  
  # Confront data with rules
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail/NA counts from report
  in_domain <- is.na(submitted_data[["DurationMethod"]]) |
    (toupper(submitted_data[["DurationMethod"]]) %in% domain_codes)
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na     <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$DurationMethod[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "DurationMethod(s) passed all validation checks."
    } else {
      "DurationMethod(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}



#' Load User Data - Validate Season
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all seasons are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' validateSeason(UTAHDWQ)
#'
# Validate Season (final format; NA allowed as pass)
validateSeason <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    submitted_data <- utils::read.csv(data, stringsAsFactors = FALSE)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  # Ensure required column exists
  if (!"Season" %in% names(submitted_data)) {
    stop("Required column 'Season' is missing from the input.")
  }
  
  # Normalize type
  submitted_data$Season <- as.character(submitted_data$Season)
  
  # Allowed values
  domain <- c("Summer", "Fall", "Spring", "Winter")
  domain_codes <- toupper(domain)
  
  # Build validation rule (NA allowed)
  rules_values <- validate::validator(
    is.na(Season) | toupper(Season) %in% domain_codes
  )
  
  # Confront data with rules
  out <- validate::confront(
    submitted_data,
    rules_values,
    ref = list(domain_codes = domain_codes)
  )
  
  # Generate validation report
  report <- validate::summary(out)
  
  # Compute pass/fail/NA counts from report
  in_domain <- is.na(submitted_data[["Season"]]) |
    (toupper(submitted_data[["Season"]]) %in% domain_codes)
  nrows_passes <- report$passes
  nrows_fails  <- report$fails
  nrows_na     <- report$nNA
  
  # Determine acceptance
  vals <- validate::values(out)
  has_rule_error <- any(isTRUE(report$error))
  accepted <- (nrows_fails == 0L) && !has_rule_error && isTRUE(all(vals))
  
  status <- if (accepted) "Accepted" else "Rejected"
  
  # Identify problematic entries (excluding NA)
  issues <- unique(stats::na.omit(submitted_data$Season[!in_domain]))
  
  # Build result
  list(
    status = status,
    message = if (accepted) {
      "Season(s) passed all validation checks."
    } else {
      "Season(s) failed some validation checks. Please review the issues."
    },
    report = report,
    issues = issues,
    nrows_fails = nrows_fails,
    nrows_passes = nrows_passes,
    nrows_na = nrows_na
  )
}


# Run all TADA/ATTAINS/WQX validation functions on a single dataset
runAllValidations <- function(
    data,
    validators = NULL,
    stop_on_error = FALSE
) {
  # Default set of validator functions (named for clarity in the output)
  if (is.null(validators)) {
    validators <- list(
      ATTAINSParam     = validateATTAINSParam,
      WQXChar          = validateWQXChar,
      ATTAINSUse       = validateATTAINSUse,
      ATTAINSOrg       = validateATTAINSOrg,
      WQXUnits         = validateWQXUnits,
      DurationUnits    = validateDurationUnits,
      FreqMethod       = validateFreqMethod,
      DurationMethod   = validateDurationMethod,
      Season           = validateSeason
    )
  }
  
  # Safety wrapper to handle errors per validator
  safe_call <- function(fun, data) {
    tryCatch(
      fun(data),
      error = function(e) {
        if (isTRUE(stop_on_error)) stop(e)
        list(
          status = "Error",
          message = paste0("Validation failed: ", conditionMessage(e)),
          report = NULL,
          issues = NULL,
          nrows_fails = NA_integer_,
          nrows_passes = NA_integer_,
          nrows_na = NA_integer_
        )
      }
    )
  }
  
  # Run each validator and collect results
  results <- lapply(validators, safe_call, data = data)
  
  # Create a compact summary data frame
  summary <- data.frame(
    validator    = names(results),
    status       = vapply(results, function(x) if (!is.null(x$status)) x$status else NA_character_, character(1)),
    nrows_passes = vapply(results, function(x) if (!is.null(x$nrows_passes)) x$nrows_passes else NA_integer_, integer(1)),
    nrows_fails  = vapply(results, function(x) if (!is.null(x$nrows_fails))  x$nrows_fails  else NA_integer_, integer(1)),
    nrows_na     = vapply(results, function(x) if (!is.null(x$nrows_na))     x$nrows_na     else NA_integer_, integer(1)),
    issues_count = vapply(results, function(x) {
      if (is.null(x$issues)) NA_integer_ else length(unique(x$issues))
    }, integer(1)),
    stringsAsFactors = FALSE
  )
  
  # Determine overall status: Accepted only if all are Accepted and none are Rejected/Error
  overall_status <- if (
    all(summary$status == "Accepted", na.rm = TRUE) &&
    !any(summary$status %in% c("Rejected", "Error"), na.rm = TRUE)
  ) "Accepted" else "Rejected"
  
  # Return a structured list
  list(
    overall_status = overall_status,
    summary = summary,
    results = results
  )
}








#' Validate all data .xlsx in a Folder Path
#'
#' For each criteria tables submitted to a folder path (defaults to those submitted
#' to the inst/extdata folder path of this TADACommunityHub repository) this will
#' validate all criteria table for a single column.
#'
#' @param folder_path The default is "inst/extdata/" to review user submitted criteria
#' table to the TADACommunityHub repository for review.
#'
#' @param validateColumn an R TADACommunityHub validate function. See `validateWQXChar()`,
#' `validateATTAINSParam`, `validateATTAINSUse` and `validateATTAINSOrg`.
#'
#' @return A list of list of what column name contains the current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#'
#' @examples
#' review <- validateAll(validateColumn = validateWQXChar)
#'
validateAll <- function(folder_path = NULL, validateColumn) {
  if (is.null(folder_path)) {
    print("No folder path specified, searching through all files currently found in inst/extdata/")
    folder_path <- system.file("extdata", package = "TADACommunityHub")
  }

  if (is.null(validateColumn)) {
    stop("You must select a column in your criteria and methodology table to validate.")
  }

  file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

  my_function <- function(x) {
    data <- readxl::read_excel(x)
    do.call(validateColumn, list(data))
  }

  safe_my_function <- purrr::possibly(my_function, otherwise = NULL)

  val_checks <- purrr::map(file_list, safe_my_function)

  names(val_checks) <- gsub("inst/extdata/", "", file_list)
  
  # df_counts <- df |>
  #   mutate(
  #     count_accepted = purrr::map_int(status, ~ sum(.x == "Accepted")),
  #     count_rejected = purrr::map_int(status, ~ sum(.x == "Rejected"))
  #   )
  # 
  # print(df_counts)
  return(val_checks)
}



#' Export data with errors from validateAll
#'
#' Loads the list of unique errors in a column and exports it to df
#' @param data a list of list of multiple data frame that is an output from
#' the TADACommunityHub R validateAll function.
#'
#' @param folder_path The default is "inst/extdata/" to review user submitted criteria
#' table to the TADACommunityHub repository for review.
#'
#' @param excel A boolean value. If TRUE, this will generate an excel spreadsheet
#' of all criteria tables in your defined folder to indicate what values not
#' a valid entry in TADA format.
#'
#' @return An excel spreadsheet that shows the invalid column values from the
#' user supplied criteria table(s). Users can choose from a drop down list of
#' allowable valid values for that column name.
#'
#' @export
#'
#' @examples
#' review2 <- validateAll(validateColumn = validateATTAINSUse)
#' err <- exportErrors(review2)
#'
exportErrors <- function(data, folder_path = NULL, excel = FALSE) {
  # Create an empty list to store the dataframes
  list_of_dataframes <- list()

  # Consider flexibility in folder path in future.
  if (is.null(folder_path)) {
    print("No folder path specified, searching through all files currently found in inst/extdata/")
    folder_path <- system.file("extdata", package = "TADACommunityHub")
  }
  file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

  # Loop through each XLSX file and read it into a dataframe, then add to the list
  for (file_path in file_list) {
    # Extract the file name without extension to use as a list element name
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # Read the Excel file into a dataframe
    df <- readxl::read_excel(file_path)

    # Add the dataframe to the list, using the file name as the element name
    list_of_dataframes[[file_name]] <- df
  }

  errors <- purrr::map(data, ~ .x$issues)

  errors_col <- names(errors[[1]])

  # Filter list of df by errors_col
  list_of_dataframes <- purrr::map(list_of_dataframes, ~ {
    if (errors_col %in% colnames(.x)) {
      unique(.x[, errors_col])
    } else {
      .x[, errors_col] <- NA
    }
    # return(.x)
  })

  # Subset each data frame
  result_list <- errors

  if (excel == TRUE) {
    # 1) openxlsx tab max length is 31 char
    n <- nchar(folder_path) - 11
    names(result_list) <- substr(names(err), 35, nchar(names(err)))
    names(result_list) <- substr(names(result_list), 1, 30)

    downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")

    file_name <- "my_exported_data.xlsx"

    full_path <- file.path(downloads_path, file_name)

    openxlsx::write.xlsx(result_list, file = full_path)

    # 2. Open the target workbook
    wb <- openxlsx::loadWorkbook(full_path)

    # 3. Get the names of all sheets in the workbook
    sheet_names <- names(wb)

    # 4. Get ATTAINS Parameter domain
    if (errors_col == "ATTAINS.ParameterName") {
      list_values <- as.character(rExpertQuery::EQ_DomainValues(domain = "param_name")[, "code"])
      openxlsx::addWorksheet(wb, "Index", visible = TRUE)
      openxlsx::writeData(
        wb,
        "Index",
        startCol = 1,
        x = list_values
      )
    }

    if (errors_col == "ATTAINS.UseName") {
      list_values <- as.character(rExpertQuery::EQ_DomainValues(domain = "use_name")[, "code"])
      openxlsx::addWorksheet(wb, "Index", visible = TRUE)
      openxlsx::writeData(
        wb,
        "Index",
        startCol = 1,
        x = list_values
      )
    }

    n_sheets <- length(wb$worksheets) - 1
    # m <- ifelse(nrow(result_list[[i]]) == 0, 1, nrow(result_list[[i]]) + 1)

    for (i in 1:n_sheets) {
      if (errors_col == "ATTAINS.ParameterName") {
        openxlsx::writeData(
          wb,
          sheet = sheet_names[i],
          startCol = 2,
          x = "Suggested.ATTAINS.ParameterName"
        )
      }
      if (errors_col == "ATTAINS.UseName") {
        openxlsx::writeData(
          wb,
          sheet = sheet_names[i],
          startCol = 2,
          x = "Suggested.ATTAINS.UseName"
        )
      }

      # openxlsx::conditionalFormatting(
      #   wb,
      #   sheet = sheet_names[i],
      #   cols = 2,
      #   rows = 1:50,
      #   type = "blanks",
      #   style = openxlsx::createStyle(bgFill = "red")
      # )

      openxlsx::conditionalFormatting(
        wb,
        sheet = sheet_names[i],
        cols = 2,
        rows = 1:50,
        type = "notBlanks",
        style = openxlsx::createStyle(bgFill = "green")
      )

      # Apply data validation to the second column (col = 2) for a range of rows
      # For example, rows 2 to 100
      openxlsx::dataValidation(
        wb,
        sheet = sheet_names[i],
        cols = 2,
        rows = 2:1000, # Adjust the row range as needed
        type = "list",
        value = sprintf("'Index'!$A$2:$A$20000")
      )
    }

    openxlsx::saveWorkbook(wb, full_path, overwrite = TRUE)
  }
  return(result_list)
}

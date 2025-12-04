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
    # Example: Read CSV
    submitted_data <- read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  rules_values <- validate::validator(
    toupper(ATTAINS.ParameterName) %in% toupper(spsUtil::quiet(rExpertQuery::EQ_DomainValues("param_name")[,"code"]))
  )
  
  # Confront data with rules
  out2 <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report2 <- validate::summary(out2)
  
  # Determine acceptance/rejection
  if (all( validate::values(out2))) { # Example: All rules passed
    result2 <- list(status = "Accepted", report = report2)
  } else {
    result2 <- list(status = "Rejected", report = report2)
  }
  
  # display message if accepted vs rejected
  if (result2$status == "Accepted") {
    result2 <- list(status = "Accepted", message = "ATTAINS.ParameterName(s) passed all validation checks.")
  } else {
    result2 <- list(status = "Rejected", message = "ATTAINS.ParameterName(s) failed some validation checks. Please review the issues.")
  }
  
  result2$issues <- unique(validate::violating(submitted_data, out2)[,"ATTAINS.ParameterName"])
  result2$nrows_fails <- report2$fails
  result2$nrows_passes <- report2$passes
    
  return(result2)
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
validateWQXChar <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  rules_values <- validate::validator(
    toupper(TADA.CharacteristicName) %in% toupper(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))[,"Name"])
  )
  
  # Confront data with rules
  out2 <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report2 <- validate::summary(out2)
  
  # Determine acceptance/rejection
  if (all( validate::values(out2))) { # Example: All rules passed
    result2 <- list(status = "Accepted", report = report2)
  } else {

    result2 <- list(status = "Rejected", report = report2)
  }
  
  # display message if accepted vs rejected
  if (result2$status == "Accepted") {
    result2 <- list(status = "Accepted", message = "WQX.CharacteristicName(s) passed all validation checks.")
  } else {
    result2 <- list(status = "Rejected", message = "WQX.CharacteristicName(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result2$issues <- unique(validate::violating(submitted_data, out2)[,"TADA.CharacteristicName"])
  result2$nrows_fails <- report2$fails
  result2$nrows_passes <- report2$passes
  
  return(result2)
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
#' data("UTAHDWQ")
#' validateATTAINSUse(UTAHDWQ)
#' 
validateATTAINSUse <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  rules_values <- validate::validator(
    toupper(ATTAINS.UseName) %in% toupper(spsUtil::quiet(rExpertQuery::EQ_DomainValues("use_name")[,"code"]))
  )
  
  # Confront data with rules
  out2 <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report2 <- validate::summary(out2)
  
  # Determine acceptance/rejection
  if (all( validate::values(out2))) { # Example: All rules passed
    result2 <- list(status = "Accepted", report = report2)
  } else {
    
    result2 <- list(status = "Rejected", report = report2)
  }
  
  # display message if accepted vs rejected
  if (result2$status == "Accepted") {
    result2 <- list(status = "Accepted", message = "ATTAINS.UseName(s) passed all validation checks.")
  } else {
    result2 <- list(status = "Rejected", message = "ATTAINS.UseName(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result2$issues <- unique(validate::violating(submitted_data, out2)[,"ATTAINS.UseName"])
  result2$nrows_fails <- report2$fails
  result2$nrows_passes <- report2$passes
  
  return(result2)
}



#' Load User Data - Validate ATTAINS Org Names
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list returning if all ATTAINS organization names are current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#' 
#' @examples
#' data("UTAHDWQ")
#' validateATTAINSOrg(UTAHDWQ)
#' 
validateATTAINSOrg <- function(data) {
  # Load or read data if a file path is provided
  if (is.character(data)) {
    # Example: Read CSV
    submitted_data <- read.csv(data)
  } else if (is.data.frame(data)) {
    submitted_data <- data
  } else {
    stop("Input 'data' must be a data frame or a file path.")
  }
  
  rules_values <- validate::validator(
    toupper(ATTAINS.OrganizationIdentifier) %in% toupper(spsUtil::quiet(rExpertQuery::EQ_DomainValues("org_id")[,"code"]))
  )
  
  # Confront data with rules
  out2 <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  report2 <- validate::summary(out2)
  
  # Determine acceptance/rejection
  if (all( validate::values(out2))) { # Example: All rules passed
    result2 <- list(status = "Accepted", report = report2)
  } else {
    
    result2 <- list(status = "Rejected", report = report2)
  }
  
  # display message if accepted vs rejected
  if (result2$status == "Accepted") {
    result2 <- list(status = "Accepted", message = "ATTAINS.OrganizationIdentifier(s) passed all validation checks.")
  } else {
    result2 <- list(status = "Rejected", message = "ATTAINS.OrganizationIdentifier(s) failed some validation checks. Please review the issues.")
  }
  
  # add values to list
  result2$issues <- unique(validate::violating(submitted_data, out2)[,"ATTAINS.OrganizationIdentifier"])
  result2$nrows_fails <- report2$fails
  result2$nrows_passes <- report2$passes
  
  return(result2)
}


#' Validate all data .xlsx in a Folder Path
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list of list of what column name contains the current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#' 
#' @examples
#' data("UTAHDWQ")
#' review <- validateAll(folder_path = "inst/extdata/", validateColumn = validateWQXChar)
#' 
validateAll <- function(folder_path = NULL, validateColumn){
  
  if ( is.null(folder_path)){
    print("No folder path specified, searching through all files currently found in inst/extdata/")
    folder_path <- "inst/extdata/"
  }
  
  if ( is.null(validateColumn)){
    stop("You must select a column in your criteria and methodology table to validate.")
  }
  
  file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  val_checks <- map(file_list, ~ {
    data <- readxl::read_excel(.x)
    do.call(validateColumn, list(data))
  })
  
  names(val_checks) <- gsub("inst/extdata/", "", file_list)
  return(val_checks)
}

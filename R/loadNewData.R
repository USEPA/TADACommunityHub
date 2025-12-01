#' Load User Data - Validate ATTAINS.ParameterName
#'
#' Loads a data frame provided by the user.
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A data frame containing the loaded data.
#' @export
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
  
  # Test rule
  # rules <- validate::validator(
  #   nrow(.) >= 5
  # )
  
  rules_values <- validate::validator(
    toupper(ATTAINS.ParameterName) %in% toupper(rExpertQuery::EQ_DomainValues("param_name")[,"code"])
  )
  
  # Confront data with rules
  #out <- validate::confront(submitted_data, rules)
  out2 <- validate::confront(submitted_data, rules_values)
  
  # Generate validation report
  #report <- validate::summary(out)
  report2 <- validate::summary(out2)
  
  # Determine acceptance/rejection
  if (all( validate::values(out2))) { # Example: All rules passed
    #result <- list(status = "Accepted", report = report)
    result2 <- list(status = "Accepted", report = report2)
  } else {
    #result <- list(status = "Rejected", report = report)
    result2 <- list(status = "Rejected", report = report2)
  }
  
  # Placeholder for demonstration
  # validation_passed <- sample(c(TRUE, FALSE), 1) # Simulate validation
  if (result2$status == "Accepted") {
    result2 <- list(status = "Accepted", message = "ATTAINS.ParamerterName(s) passed all validation checks.")
  } else {
    result2 <- list(status = "Rejected", message = "ATTAINS.ParamerteName(s) failed some validation checks. Please review the issues.")
  }
  
  # val <- validate::violating(submitted_data, out2)
  result2$issues <- unique(as.character(validate::violating(submitted_data, out2)[,"ATTAINS.ParameterName"]))
  result2$nrows_fails <- report2$fails
  result2$nrows_passes <- report2$passes
    
  return(result2)
}

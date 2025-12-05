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



#' Export data with errors
#'
#' Loads the list of unique errors in a column and exports it to df
#' @param data a R data frame. Future dev will allow other data file types.
#' @return A list of list of what column name contains the current valid
#' domain values or not. If not, identify which are not valid.
#' @export
#' 
#' @examples
#' data("UTAHDWQ")
#' review <- validateAll(folder_path = "inst/extdata/", validateColumn = validateWQXChar)
#' 
exportErrors <- function(data, df_return = TRUE){
  
  if ( is.null(folder_path)){
    print("No folder path specified, searching through all files currently found in inst/extdata/")
    folder_path <- "inst/extdata/"
  }
  
  file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  # Create an empty list to store the dataframes
  list_of_dataframes <- list()
  
  # Loop through each XLSX file and read it into a dataframe, then add to the list
  for (file_path in file_list) {
    # Extract the file name without extension to use as a list element name
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Read the Excel file into a dataframe
    df <- read_excel(file_path)
    
    # Add the dataframe to the list, using the file name as the element name
    list_of_dataframes[[file_name]] <- df
  }
  
  # View the result
  print(list_of_dataframes)
  
  errors <- purrr::map(data, ~.x$issues)
  
  errors_col <- names(errors[[1]])
  
  # Filter list of df by errors_col
  list_of_dataframes <- purrr::map(list_of_dataframes, ~unique(.x[,errors_col]))
  
  # Subset each data frame
  result_list <- map2(list_of_dataframes, errors, ~ {
    if (length(.y$ATTAINS.ParameterName) == 0) {
      .x # Return original dataframe if filter list is empty
    } else {
      .x[.x$ATTAINS.ParameterName %in% .y$ATTAINS.ParameterName, ]
    }
  })

  # 1) openxlsx tab max length is 31 char
  names(result_list) <- substr(names(result_list), 1, 20)
  
  downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  
  file_name <- "my_exported_data.xlsx"
  
  full_path <- file.path(downloads_path, file_name)
  
  openxlsx::write.xlsx(result_list, file = full_path)
  
  # 2. Open the target workbook
  wb <- openxlsx::loadWorkbook(full_path)
  
  # 3. Get the names of all sheets in the workbook
  sheet_names <- names(wb)
  
  # 4. Get ATTAINS Parameter domain
  list_values <- as.character(rExpertQuery::EQ_DomainValues(domain = "param_name")[,"code"])
  openxlsx::addWorksheet(wb, "Index", visible = TRUE)
  openxlsx::writeData(
    wb,
    "Index",
    startCol = 1,
    x = list_values
  )
  
  n_sheets <- length(wb$worksheets) -1
  
  for (i in 1:n_sheets) {
    openxlsx::writeData(
      wb,
      sheet = sheet_names[i],
      startCol = 2,
      x = "Suggested.ATTAINS.ParameterName"
    )
    
    openxlsx::conditionalFormatting(
      wb,
      sheet = sheet_names[i],
      cols = 2,
      rows = 1:nrow(result_list[[i]]) + 1,
      type = "blanks",
      style = openxlsx::createStyle(bgFill = "red")
    )
    
    openxlsx::conditionalFormatting(
      wb,
      sheet = sheet_names[i],
      cols = 2,
      rows = 1:nrow(result_list[[i]]) + 1,
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
      value = sprintf("'Index'!$A$2:$A$1100")
    )
  }
  
  openxlsx::saveWorkbook(wb, full_path, overwrite = TRUE)
  
  return(result_list)
}

#' Load Criteria - the function loads the criteria table
#' based on the state and tribe name and link to the public GitHub space
#'
#' @param state_tribe the state or tribe name
#' @param ref the reference table showing the state and tribe name
#' and their file name
#' @return the criteria table as a data frame
#'

loadCriteria <- function(state_tribe, ref) {
  # Base URL
  base_url <- "https://raw.githubusercontent.com/USEPA/TADACommunityHub/crosswalks/inst/extdata/"

  # Get the file name
  file_name <- ref[ref$`Display Name` %in% state_tribe, "File"]

  # Url
  url <- paste0(base_url, file_name, "_criteria_crosswalk.xlsx")

  # Create a temporary file
  temp_file <- tempfile(fileext = ".xlsx")

  # Download the file (use mode = "wb" for binary files like xlsx)
  utils::download.file(url, temp_file, mode = "wb")

  # Now read it
  df <- readxl::read_excel(temp_file)

  # Clean up
  unlink(temp_file)

  return(df)
}

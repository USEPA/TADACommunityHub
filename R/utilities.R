# write global variables. Gets rid of global variable NOTE in check:
utils::globalVariables(c(
  "ATTAINS.OrganizationIdentifier",
  "ATTAINS.ParameterName",
  "ATTAINS.UseName",
  "TADA.CharacteristicName",
  "err",
  "DurationMethod",
  "DurationUnit",
  "FreqMethod",
  "MagnitudeUnit",
  "Season"
))

#' .setEQKey
#' Resolve the rExpertQuery API key, preferring env/options over default
#' @return Expert Query API key for use in TADACommunityHub functions, checks, or tests.
.setEQKey <- function() {
  # check to see if key is stored in R session
  # this allows developers to easily use their own key during local dev and testing
  # per session: options(rexpertquery.eq_key = "YOUR_KEY_HERE")
  # use options(rexpertquery.eq_key = NULL) to remove
  opt <- getOption("EQ_API_KEY", "")
  if (nzchar(opt)) {
    return(opt)
  }

  # check to see if key is stored in system environment (primarily for use in checks)
  env <- Sys.getenv("EQ_API_KEY", unset = "")
  if (nzchar(env)) {
    return(env)
  }

  # if neither exist
  def <- "lfzVzpwIlKS1O4l1QmbOLUeTzxyql4QdbHVR5Yf5"
  if (nzchar(def)) {
    return(def)
  }
}

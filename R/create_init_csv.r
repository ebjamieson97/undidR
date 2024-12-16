#' Creates the `init.csv`
#'
#' The `create_init_csv` function generates a CSV file with information
#' on each silo's start times, end times, and treatment times.
#' If parameters are left empty, generates a blank CSV with only the headers.
#'
#' @details Ensure dates are entered consistently in the same date format.
#' Control silos should be marked as `"control"` in the `treatment_times`
#' vector. If `covariates` is `FALSE`, no covariate column will be
#' included in the CSV.
#'
#' @param silo_names A vector of strings of silo names.
#' @param start_times A vector of strings of start times.
#' @param end_times A vector of strings of end times.
#' @param treatment_times A vector of strings of treatment times.
#' @param covariates A vector of strings of covariates (optional).
#' @param filename A string filename for the created initializing .csv file.
#' Defaults to `init.csv`
#' @param filepath Filepath to save the .csv file. Defaults to `tempdir()`.
#'
#' @return A data frame containing the contents written to the CSV file.
#' The CSV file is saved in the specified directory (or in a temporary
#' directory by default) with the default filename `init.csv`.
#'
#' @export
#'
#' @examples
#' create_init_csv(
#'   silo_names = c("71", "73"),
#'   start_times = c("1989", "1989"),
#'   end_times = c("2000", "2000"),
#'   treatment_times = c("1991", "control"),
#'   covariates = c("asian", "black", "male")
#' )
#' unlink(file.path(tempdir(), "init.csv"))
#'
create_init_csv <- function(silo_names = character(), start_times = character(),
                            end_times = character(),
                            treatment_times = character(),
                            covariates = character(), filename = "init.csv",
                            filepath = tempdir()) {

  # Run filepath and filename checks
  filepath <- .filename_filepath_check(filename, filepath)

  # Return character() for covariates if covariates is null,
  # FALSE, or whitespace
  covariates <- .init_covariates_check(covariates)

  # Check if each argument is of type 'character'
  args <- list(silo_names = silo_names, start_times = start_times,
               end_times = end_times, treatment_times = treatment_times,
               covariates = covariates)
  for (arg_name in names(args)) {
    if (!is.character(args[[arg_name]])) {
      stop(paste("Error:", sQuote(arg_name), "must be of type character."))
    }
  }

  # Remove whitespace typos from start_times, end_times, and treatment_times
  start_times <- gsub("\\s+", "", start_times)
  end_times <- gsub("\\s+", "", end_times)
  treatment_times <- gsub("\\s+", "", treatment_times)

  # If only a single value is entered for start or end times, adjust to vector
  if (length(start_times) == 1) {
    start_times <- rep(start_times, length(silo_names))
  }
  if (length(end_times) == 1) {
    end_times <- rep(end_times, length(silo_names))
  }

  # First ensure that silo_names and treatment times have the same length
  vector_lengths <- vapply(list(silo_names, start_times, end_times,
                                treatment_times), length, integer(1))
  if (length(unique(vector_lengths)) != 1) {
    stop("Error: 'silo_names' and 'treatment_times' must both have
     the same length, and 'start_times' and 'end_times' must either have
     a length of 1 or the same length as 'silo_names' and 'treatment_times'.")
  }

  # Parse covariates
  if (length(covariates) > 0) {
    covariates_combined <- paste(covariates, collapse = ";")
    covariates_repeated <- rep(covariates_combined, length(silo_names))
  } else {
    covariates_repeated <- NULL
  }

  # Create init dataframe
  if (!is.null(covariates_repeated)) {
    init <- data.frame(silo_name = silo_names, start_time = start_times,
                       end_time = end_times, treatment_time = treatment_times,
                       covariates = covariates_repeated)
  } else {
    init <- data.frame(silo_name = silo_names, start_time = start_times,
                       end_time = end_times, treatment_time = treatment_times)
  }

  # Force control entries to lowercase
  if (nrow(init) > 0) {
    init$treatment_time <- tolower(init$treatment_time)
  }

  # Run init logic checks
  .init_checks(init)

  full_path <- file.path(filepath, filename)
  # Save as csv, print filepath, return dataframe
  write.csv(init, full_path, row.names = FALSE, quote = FALSE,
            fileEncoding = "UTF-8")
  message(filename, " saved to: ", full_path)
  return(init)
}
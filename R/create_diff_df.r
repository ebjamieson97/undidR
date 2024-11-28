#' Creates the `empty_diff_df.csv`
#'
#' Creates the `empty_diff_df.csv` which lists all of the differences that
#' need to calculated at each silo in order to compute the aggregate ATT.
#' The `empty_diff_df.csv` is then sent out to each silo to be filled out.
#'
#' @details Ensure that dates in the `init.csv` are entered consistently
#' in the same date format. Call `undid_date_formats()` to see a list of valid
#' date formats. Covariates specified when calling `create_diff_df` will
#' override any covariates specified in the `init.csv`.
#'
#' @param init_filepath A string filepath to the `init.csv`.
#' @param date_format A string specifying the date format used in the
#' `init.csv`. Call `undid_date_formats()` to see a list of valid date formats.
#' @param freq A string indicating the length of the time periods to be used
#' when computing the differences in mean outcomes between periods at each silo.
#' @param covariates A vector of strings specifying covariates to be considered
#' at each silo. If `FALSE` uses covariates from the `init.csv`.
#' Defaults to `FALSE`.
#' @param freq_multiplier A numeric value or `FALSE`. Specify if the frequency
#' should be multiplied by a non-zero integer. For example, to consider two year
#' periods, set `freq = "yearly", freq_multiplier = 2`. Defaults to `FAlSE`.
#' @param weights A string indicating the type of weighting to use in the
#' case of common adoption. Defaults to `"standard"`. The `"standard"` weight
#' is calculated as \eqn{w_s = \frac{N_s^{\text{post}}}{N_s^{\text{post}} + N_s^{\text{pre}}}}.
#' @param filename A string filename for the created .csv file. Defaults to
#' `empty_diff_df.csv`
#' @param filepath Filepath to save the .csv file. Defaults to `tempdir()`.
#'
#' @return The `empty_diff_df.csv` is created in the working directory.
#' Its file path is printed and a dataframe of the `empty_diff_df.csv`
#' is returned.
#'
#'
#' @examples
#' # Create a temporary `init.csv` file for this example
#' init_df <- data.frame(
#'   silo_name = c("71", "73"),
#'   start_time = c("1989", "1989"),
#'   end_time = c("2000", "2000"),
#'   treatment_time = c("1991", "control")
#' )
#' init_filepath <- tempfile(fileext = ".csv")
#' write.table(init_df, init_filepath, sep = ",", row.names = FALSE,
#'             col.names = TRUE, quote = FALSE)
#'
#' # Example call to create_diff_df
#' create_diff_df(
#'   init_filepath = init_filepath,
#'   date_format = "yyyy",
#'   freq = "yearly"
#' )
#' unlink(file.path(tempdir(), c("init.csv", "empty_diff_df.csv")))
#' @importFrom utils read.csv write.csv
#' @export
create_diff_df <- function(init_filepath, date_format, freq, covariates = FALSE,
                           freq_multiplier = FALSE, weights = "standard",
                           filename = "empty_diff_df.csv",
                           filepath = tempdir()) {

  # Run filepaths and filename checks
  filepath <- .filename_filepath_check(filename, filepath)


  # Force freq to lowercase
  freq <- tolower(freq)

  # Read in the init.csv, ensure everything is a string
  init_df <- read.csv(init_filepath, header = TRUE, sep = ",",
                      stringsAsFactors = FALSE)
  init_df <- data.frame(lapply(init_df, as.character), stringsAsFactors = FALSE)
  init_df$treatment_time <- tolower(init_df$treatment_time)

  # Run init logic checks
  .init_checks(init_df)

  # Convert start_time and end_time columns to date objects
  init_df$start_time <- as.Date(vapply(init_df$start_time,
                                       .parse_string_to_date,
                                       FUN.VALUE = as.Date(NA),
                                       date_format = date_format))
  init_df$end_time <- as.Date(vapply(init_df$end_time, .parse_string_to_date,
                                     FUN.VALUE = as.Date(NA),
                                     date_format = date_format))

  # Ensure that start times < treat times < end times
  .start_treat_end_time_check(init_df, date_format)


  # Process freq_multiplier and freq
  freq_string <- .parse_freq_freq_multiplier(freq, freq_multiplier)

  # Consider the case of common adoption
  if (length(unique(init_df$treatment_time)) == 2) {
    diff_df <- .create_common_diff_df(init_df, weights)

    # Consider the case of staggered adoption
  } else if (length(unique(init_df$treatment_time)) > 2) {
    diff_df <- .create_staggered_diff_df(init_df, date_format, freq_string)
  } else {
    stop("Error: only one unique treatment_time value found.")
  }

  # Add the diff_estimate columns
  diff_df$diff_estimate <- rep(NA_real_, nrow(diff_df))
  diff_df$diff_var <- rep(NA_real_, nrow(diff_df))
  diff_df$diff_estimate_covariates <- rep(NA_real_, nrow(diff_df))
  diff_df$diff_var_covariates <- rep(NA_real_, nrow(diff_df))

  # Add the covariates if they exist
  if (identical(covariates, FALSE)) {
    if ("covariates" %in% colnames(init_df)) {
      covariates <- init_df[, "covariates"][1]
    } else {
      covariates <- "none"
    }
  } else {
    covariates <- paste(covariates, collapse = ";")
  }
  diff_df$covariates <- rep(covariates, nrow(diff_df))

  # Note date_format and freq info
  if (date_format %in% .undid_env$date_formats_r) {
    date_format <- .undid_env$date_format_dict_from_r[date_format]
  }
  diff_df$date_format <- rep(date_format, nrow(diff_df))
  diff_df$freq <- rep(freq_string, nrow(diff_df))

  full_path <- file.path(filepath, filename)
  # Save as csv, print filepath, return dataframe
  write.csv(diff_df, full_path, row.names = FALSE, quote = FALSE,
            fileEncoding = "UTF-8")
  message(filename, " saved to: ", full_path)
  rownames(diff_df) <- NULL
  return(diff_df)

}

#' @keywords internal
# Process freq and freq_multiplier for create_diff_df
.parse_freq_freq_multiplier <- function(freq, freq_multiplier) {
  # Process freq_multiplier and freq
  if (!freq %in% names(.undid_env$freq_map)) {
    stop("Choose: \"yearly\", \"monthly\", \"weekly\", or \"daily\".")
  } else {
    freq <- .undid_env$freq_map[[freq]]
  }
  if (freq_multiplier == FALSE) {
    freq_multiplier <- "1"
  } else if (is.numeric(freq_multiplier)) {
    freq_multiplier <- as.character(as.integer(freq_multiplier))
    if (freq_multiplier == "0") {
      stop("Ensure freq_multiplier is set to FALSE or a non-zero integer.")
    }
    freq <- paste0(freq, "s")
  } else {
    stop("Ensure freq_multiplier is set to FALSE or a non-zero integer.")
  }
  freq_string <- paste0(freq_multiplier, " ", freq)
  return(freq_string)
}

#' @keywords internal
# Create empty_diff_df.csv for common treatment time
.create_common_diff_df <- function(init_df, weights) {
  silo_name <- c()
  treat <- c()
  common_treatment_time <- rep(init_df[init_df$treatment_time != "control",
                                       "treatment_time"][1], nrow(init_df))
  start_time <- c()
  end_time <- c()
  for (silo in unique(init_df$silo_name)) {
    if (init_df[init_df$silo_name == silo, "treatment_time"] != "control") {
      treat <- c(treat, 1)
    } else if (init_df[init_df$silo_name == silo,
                       "treatment_time"] == "control") {
      treat <- c(treat, 0)
    }
    start_time <- c(start_time, init_df[init_df$silo_name == silo,
                                        "start_time"])
    end_time <- c(end_time, init_df[init_df$silo_name == silo,
                                    "end_time"])
    silo_name <- c(silo_name, silo)
  }
  if (weights == "standard") {
    weights <- rep("standard", nrow(init_df))
  }
  diff_df <- data.frame(silo_name = silo_name, treat = treat,
                        common_treatment_time = common_treatment_time,
                        start_time = start_time, end_time = end_time,
                        weights = weights)
  diff_df$start_time <- as.Date(diff_df$start_time)
  diff_df$end_time <- as.Date(diff_df$end_time)
  return(diff_df)
}

#' @keywords internal
# Create empty_diff_df.csv for staggered adoption
.create_staggered_diff_df <- function(init_df, date_format, freq_string) {

  # First grab all unique treatment times
  init_df$treatment_time_date <- vapply(init_df$treatment_time, function(x) {
    if (x != "control") {
      return(.parse_string_to_date(x, date_format = date_format))
    } else if (x == "control") {
      return(NA)
    }
  }, FUN.VALUE = as.Date(NA))
  all_treatment_times <- sort(as.Date(na.omit(init_df$treatment_time_date)))

  # Grab start and end times
  start_window <- as.character(init_df$start_time[1])
  end_window <- as.character(init_df$end_time[1])

  # Create an empty list to store diff_df subsets for each silo
  diff_df_list <- list()
  # Loop through each silo and create appropriate rows
  for (silo in unique(init_df$silo_name)) {
    start <- init_df[init_df$silo_name == silo, "start_time"]
    end <- init_df[init_df$silo_name == silo, "end_time"]
    times <- seq(from = start, to = end, by = freq_string)
    treatment_time <- init_df[init_df$silo_name == silo, "treatment_time"]

    if (treatment_time != "control") {
      treatment_time <- .parse_string_to_date(treatment_time, date_format)
      gt <- expand.grid(g = treatment_time, t = times)
      gt <- subset(gt, g <= t)
      diff_times <- expand.grid(post = seq(from = treatment_time, to = end,
                                           by = freq_string),
                                pre = seq(treatment_time, length = 2,
                                          by = paste0("-", freq_string))[2])
      treat <- "1"
    } else if (treatment_time == "control") {
      gt <- expand.grid(g = all_treatment_times, t = times)
      gt <- subset(gt, g <= t)
      gt <- gt[order(gt$g), ]
      gt <- unique(gt)
      diff_times <- data.frame(post = as.Date(NULL), pre = as.Date(NULL))
      for (g in unique(gt$g)) {
        g <- as.Date(g)
        post_periods <- seq(from = g, to = end, by = freq_string)
        pre_period <- seq(g, length = 2,
                          by = paste0("-", freq_string))[2]
        diff_times <- rbind(diff_times,
                            expand.grid(post = post_periods,
                                        pre = pre_period))
      }
      treat <- "0"
    }
    # Define number of rows for the subset, parse dates to strings
    diff_times_nrows <- nrow(diff_times)
    gvar_vector <- gt$g
    t_vector <- .parse_date_to_string(gt$t, date_format)
    post_vector <- .parse_date_to_string(diff_times$post, date_format)
    pre_vector <- .parse_date_to_string(diff_times$pre, date_format)
    # Create the diff_df subset for that silo
    temp_df <- data.frame(silo_name = rep(silo, diff_times_nrows),
                          gvar = gvar_vector,
                          treat = rep(treat, diff_times_nrows),
                          diff_times = paste(post_vector,
                                             pre_vector, sep = ";"),
                          gt = paste(.parse_date_to_string(gvar_vector,
                                                           date_format),
                                     t_vector, sep = ";"),
                          stringsAsFactors = FALSE)
    # Store the diff_df subset for each silo in list
    diff_df_list[[length(diff_df_list) + 1]] <- temp_df
  }
  # Combine all the diff_df subsets of each silo into a final diff_df
  diff_df <- do.call(rbind, diff_df_list)

  # Now add rows for RI
  diff_df$RI <- rep(0, nrow(diff_df))
  diff_df <- diff_df[order(diff_df$gvar), ]
  ri_df_list <- list()
  control_silo <- diff_df[diff_df$treat == "0", "silo_name"][1]
  for (silo in unique(diff_df[diff_df$treat == "1", "silo_name"])) {
    gvar <- unique(diff_df[diff_df$silo_name == silo, "gvar"])
    gvars <- diff_df[
      (diff_df$gvar != gvar) & (diff_df$silo_name == control_silo), "gvar"
    ]
    diff_times <- diff_df[
      (diff_df$gvar != gvar) & (diff_df$silo_name == control_silo), "diff_times"
    ]
    gt <- diff_df[
      (diff_df$gvar != gvar) & (diff_df$silo_name == control_silo), "gt"
    ]
    n <- length(gt)
    temp_df <- data.frame(silo_name = rep(silo, n), gvar = gvars,
                          treat = rep(-1, n), diff_times = diff_times, gt = gt,
                          RI = rep(1, n))
    ri_df_list[[length(ri_df_list) + 1]] <- temp_df
  }
  ri_df <- do.call(rbind, ri_df_list)
  diff_df <- rbind(diff_df, ri_df)
  diff_df$gvar <- .parse_date_to_string(diff_df$gvar, date_format)
  diff_df$start_time <- start_window
  diff_df$end_time <- end_window
  return(diff_df)
}
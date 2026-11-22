# Creates an empty_diff_df.csv in tempdir() from one of the example init.csv
# files, using the given covariates, and returns its filepath
make_empty_diff <- function(setting, covariates) {
  init <- system.file("extdata", setting, "init.csv", package = "undidR")
  fname <- paste0("empty_diff_", setting, "_",
                  paste(covariates, collapse = "_"), ".csv")
  suppressMessages(create_diff_df(init, "yyyy", "yearly",
                                  covariates = covariates, filename = fname))
  file.path(tempdir(), fname)
}

# Runs undid_stage_two() for silo 71 without printing file save messages
run_stage_two_71 <- function(empty_diff_filepath, silo_data, ...) {
  suppressMessages(undid_stage_two(empty_diff_filepath, "71", silo_data,
                                   "year", "coll", "yyyy", ...))
}

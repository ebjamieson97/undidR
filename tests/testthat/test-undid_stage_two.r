test_that("`undid_stage_two()` processes staggered adoption properly",
  {
    empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                       package = "undidR")
    df <- undid_stage_two(empty_diff_filepath, "71", silo71, "year", "coll",
                          "yyyy")
    expect_equal(df$diff_df$diff_estimate[1], 0.129166666666667)
    expect_equal(df$diff_df$diff_estimate_covariates[1], 0.116348471515476)
    expect_equal(df$trends_data$mean_outcome[1], 0.306122448979592)
    expect_equal(df$trends_data$mean_outcome_residualized[1], 0.199879951980792)
  }
)

test_that("`undid_stage_two()` processes common adoption properly",
  {
    empty_diff_filepath <- system.file("extdata/common", "empty_diff_df.csv",
                                       package = "undidR")
    df <- undid_stage_two(empty_diff_filepath, "71", silo71, "year", "coll",
                          "yyyy")
    expect_equal(df$diff_df$diff_estimate[1], 0.0587978333042113)
  }
)

test_that("argument consider_covariates works as intended",
  {
    empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                       package = "undidR")
    df <- undid_stage_two(empty_diff_filepath, "71", silo71, "year", "coll",
                          "yyyy", consider_covariates = FALSE)
    expect_equal(df$diff_df$diff_estimate[1], 0.129166666666667)
    expect_equal(df$diff_df$diff_estimate_covariates[1], NA_real_)
    expect_equal(df$trends_data$mean_outcome[1], 0.306122448979592)
    expect_equal(df$trends_data$mean_outcome_residualized[1], NA_real_)
  }
)

test_that("warning displayed when some diff_estimates cannot be comptued",
  {
    empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                       package = "undidR")
    expect_warning(undid_stage_two(empty_diff_filepath, "71",
                                   silo71[silo71$year != "1993", ], "year",
                                   "coll", "yyyy", consider_covariates = FALSE))
  }
)

test_that("rows with missing values are dropped with a per-column warning",
  {
    empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                       package = "undidR")
    silo_data <- silo71
    silo_data$coll[1:3] <- NA
    silo_data$asian[3:4] <- NA
    silo_data$year[10] <- NA
    expect_warning(
      df <- undid_stage_two(empty_diff_filepath, "71", silo_data, "year",
                            "coll", "yyyy"),
      "coll: 3\n  asian: 2\n  year: 1\nDropped 5 row(s)",
      fixed = TRUE
    )
    expect_s3_class(df$diff_df, "data.frame")

    # Missing values in a character time column are dropped before parsing
    silo_data$year <- as.character(silo_data$year)
    expect_warning(
      undid_stage_two(empty_diff_filepath, "71", silo_data, "year",
                      "coll", "yyyy"),
      "year: 1",
      fixed = TRUE
    )

    silo_data$coll <- NA_real_
    expect_error(
      suppressWarnings(undid_stage_two(empty_diff_filepath, "71", silo_data,
                                       "year", "coll", "yyyy")),
      "No rows remain"
    )
  }
)

test_that(".expand_covariates() creates dummies with the expected reference",
  {
    df <- data.frame(
      chr = c("b", "a", "c", "a"),
      fac = factor(c("mid", "high", "mid", "high"),
                   levels = c("low", "mid", "high")),
      num = c(10, 2, 1, 2),
      lgl = c(TRUE, FALSE, TRUE, FALSE),
      cont = c(1.5, 2.5, 3.5, 4.5),
      stringsAsFactors = FALSE
    )
    res <- .expand_covariates(df, c("chr", "fac", "num", "lgl", "cont"),
                              factor_cols = "num")

    # Character: sorted, so "a" is the reference
    expect_equal(res$df$chr_b, c(1, 0, 0, 0))
    expect_equal(res$df$chr_c, c(0, 0, 1, 0))
    # Factor: unused lowest level "low" is dropped, so "mid" is the reference
    expect_equal(res$df$fac_high, c(0, 1, 0, 1))
    expect_false("fac_mid" %in% names(res$df))
    # Numeric declared via factor_cols: sorted numerically (not
    # alphabetically), so 1 is the reference and 2 comes before 10
    expect_equal(res$df$num_2, c(0, 1, 0, 1))
    expect_equal(res$df$num_10, c(1, 0, 0, 0))
    # Logical becomes 0/1, continuous is untouched
    expect_equal(res$df$lgl, c(1, 0, 1, 0))
    expect_equal(res$df$cont, df$cont)

    expect_equal(res$covariate_cols, c("chr_b", "chr_c", "fac_high", "num_2",
                                       "num_10", "lgl", "cont"))
    # Original categorical columns are replaced by their dummies
    expect_false(any(c("chr", "fac", "num") %in% names(res$df)))
  }
)

test_that(".expand_covariates() uses factor level order, not alphabetical",
  {
    df <- data.frame(f = factor(c("b", "a", "c"), levels = c("c", "a", "b")))
    res <- .expand_covariates(df, "f")
    expect_equal(res$covariate_cols, c("f_a", "f_b"))
  }
)

test_that(".expand_covariates() handles single categories and name clashes",
  {
    df <- data.frame(a = c("x", "x", "x"), b = c(1, 2, 3))
    expect_warning(res <- .expand_covariates(df, c("a", "b")),
                   "only one category in this silo and were dropped: a")
    expect_equal(res$covariate_cols, "b")
    expect_false("a" %in% names(res$df))

    # A dummy name that already exists as a column is made unique
    df <- data.frame(e = c("a", "b", "a"), e_b = c(5, 6, 7))
    res <- .expand_covariates(df, c("e", "e_b"))
    expect_equal(res$covariate_cols, c("e_b.1", "e_b"))
    expect_equal(res$df$e_b, c(5, 6, 7))
    expect_equal(res$df$e_b.1, c(0, 1, 0))
  }
)

test_that(".expand_covariates() does not trim whitespace from categories",
  {
    df <- data.frame(e = c("HS", " HS", "PhD"), stringsAsFactors = FALSE)
    res <- .expand_covariates(df, "e")
    expect_length(res$covariate_cols, 2)
  }
)

test_that(".blank_strings_to_na() only converts empty/whitespace values",
  {
    df <- data.frame(s = c(" a ", "", "\t", "b"),
                     f = factor(c("x", " ", "y", "x")),
                     n = c(1, 2, 3, 4),
                     stringsAsFactors = FALSE)
    res <- .blank_strings_to_na(df)
    expect_equal(res$df$s, c(" a ", NA, NA, "b"))
    expect_equal(as.character(res$df$f), c("x", NA, "y", "x"))
    expect_equal(res$df$n, df$n)
    expect_equal(res$n_blank, c(s = 2L, f = 1L))
  }
)

test_that(".parse_factor_arg() accepts strings and formulas",
  {
    expect_equal(.parse_factor_arg(NULL), character())
    expect_equal(.parse_factor_arg("educ"), "educ")
    expect_equal(.parse_factor_arg(c("educ", "race")), c("educ", "race"))
    expect_equal(.parse_factor_arg(~educ), "educ")
    three <- c("educ", "race", "sex")
    expect_equal(.parse_factor_arg(~educ + race + sex), three)
    expect_equal(.parse_factor_arg(~educ ~race ~sex), three)
    expect_equal(.parse_factor_arg(c(~educ, ~race, ~sex)), three)
    expect_equal(.parse_factor_arg(list("educ", ~race + sex)), three)
    expect_error(.parse_factor_arg(5), "'factor' must be NULL")
    expect_error(.parse_factor_arg(list(~educ, 5)), "'factor' must be NULL")
    expect_error(.parse_factor_arg(list()), "'factor' must be NULL")
  }
)

test_that(".expand_covariates() sorts strings independently of locale",
  {
    # Radix sorting uses C-locale ordering: uppercase before lowercase
    df <- data.frame(e = c("b", "B", "a", "A"), stringsAsFactors = FALSE)
    res <- .expand_covariates(df, "e")
    expect_equal(res$covariate_cols, c("e_B", "e_a", "e_b"))
  }
)

test_that("character covariates in stage two match lm() (staggered)",
  {
    empty <- make_empty_diff("staggered", c("asian", "black", "male", "educ"))
    silo_data <- silo71
    silo_data$educ <- rep_len(c("HS", "College", "PhD"), nrow(silo_data))
    res <- run_stage_two_71(empty, silo_data)

    # The first difference for silo 71 is post = 1991, pre = 1990
    expect_equal(res$diff_df$diff_times[1], "1991;1990")
    sub <- as.data.frame(silo_data[silo_data$year %in% c("1990", "1991"), ])
    sub$post <- as.numeric(sub$year == "1991")
    fit <- lm(coll ~ post + asian + black + male + educ, data = sub)
    expect_equal(res$diff_df$diff_estimate_covariates[1],
                 unname(coef(fit)["post"]))

    # Output labels keep the original covariate names
    expect_equal(res$diff_df$covariates[1], "asian;black;male;educ")
    expect_equal(res$trends_data$covariates[1], "asian;black;male;educ")
    expect_false(anyNA(res$diff_df$diff_estimate_covariates[
      res$diff_df$RI == 0
    ]))
    expect_false(anyNA(res$trends_data$mean_outcome_residualized))
  }
)

test_that("character covariates in stage two match lm() (common)",
  {
    empty <- make_empty_diff("common", c("asian", "black", "male", "educ"))
    silo_data <- silo71
    silo_data$educ <- rep_len(c("HS", "College", "PhD"), nrow(silo_data))
    res <- run_stage_two_71(empty, silo_data)

    df <- as.data.frame(silo_data)
    df$post <- as.numeric(as.integer(df$year) >= 1991)
    fit <- lm(coll ~ post + asian + black + male + educ, data = df)
    expect_equal(res$diff_df$diff_estimate_covariates[1],
                 unname(coef(fit)["post"]))
  }
)

test_that("an absent reference category does not produce NA estimates",
  {
    # "College" (the reference) never appears in 1990 or 1991, so in that
    # difference the HS and PhD dummies sum to the intercept
    empty <- make_empty_diff("staggered", c("asian", "black", "male", "educ"))
    silo_data <- silo71
    n <- nrow(silo_data)
    silo_data$educ <- ifelse(silo_data$year %in% c("1990", "1991"),
                             rep_len(c("HS", "PhD"), n),
                             rep_len(c("HS", "College", "PhD"), n))
    res <- run_stage_two_71(empty, silo_data)

    sub <- as.data.frame(silo_data[silo_data$year %in% c("1990", "1991"), ])
    sub$post <- as.numeric(sub$year == "1991")
    fit <- lm(coll ~ post + asian + black + male + educ, data = sub)
    expect_false(is.na(res$diff_df$diff_estimate_covariates[1]))
    expect_equal(res$diff_df$diff_estimate_covariates[1],
                 unname(coef(fit)["post"]))
  }
)

test_that("the factor arg treats numeric codes as categorical",
  {
    empty <- make_empty_diff("staggered", c("asian", "black", "male", "educ"))
    silo_data <- silo71
    educ_chr <- rep_len(c("HS", "College", "PhD"), nrow(silo_data))

    silo_data$educ <- educ_chr
    res_chr <- run_stage_two_71(empty, silo_data)

    silo_data$educ <- match(educ_chr, c("HS", "College", "PhD"))
    res_string <- run_stage_two_71(empty, silo_data, factor = "educ")
    res_formula <- run_stage_two_71(empty, silo_data, factor = ~educ)
    res_list <- run_stage_two_71(empty, silo_data, factor = c(~educ))
    res_continuous <- run_stage_two_71(empty, silo_data)

    # The treatment coefficient does not depend on the reference category,
    # so declaring the numeric codes as a factor matches the character version
    expect_equal(res_string$diff_df$diff_estimate_covariates,
                 res_chr$diff_df$diff_estimate_covariates)
    expect_equal(res_formula$diff_df$diff_estimate_covariates,
                 res_chr$diff_df$diff_estimate_covariates)
    expect_equal(res_list$diff_df$diff_estimate_covariates,
                 res_chr$diff_df$diff_estimate_covariates)
    # Without the factor arg, numeric codes are treated as continuous
    expect_false(isTRUE(all.equal(
      res_continuous$diff_df$diff_estimate_covariates,
      res_chr$diff_df$diff_estimate_covariates
    )))
  }
)

test_that("factor names that are not covariates are warned about and ignored",
  {
    empty <- make_empty_diff("staggered", c("asian", "black", "male", "educ"))
    silo_data <- silo71
    silo_data$educ <- rep_len(c("HS", "College", "PhD"), nrow(silo_data))
    res_ref <- run_stage_two_71(empty, silo_data)
    expect_warning(
      res <- run_stage_two_71(empty, silo_data, factor = c("educ", "merit")),
      "not covariates being considered and will be ignored: merit"
    )
    expect_equal(res$diff_df$diff_estimate_covariates,
                 res_ref$diff_df$diff_estimate_covariates)

    # With consider_covariates = FALSE nothing is a covariate
    expect_warning(
      run_stage_two_71(empty, silo_data, factor = "educ",
                       consider_covariates = FALSE),
      "will be ignored: educ"
    )
  }
)

test_that("blank strings are dropped as missing values with a warning",
  {
    empty <- make_empty_diff("staggered", c("asian", "black", "male", "educ"))
    silo_data <- silo71
    silo_data$educ <- rep_len(c("HS", "College", "PhD"), nrow(silo_data))
    silo_data$educ[1:2] <- c("", "   ")
    silo_data$year[3] <- " "
    expect_warning(
      res <- run_stage_two_71(empty, silo_data),
      paste0("year: 1 (1 empty or whitespace-only)\n",
             "  educ: 2 (2 empty or whitespace-only)\n",
             "Dropped 3 row(s)"),
      fixed = TRUE
    )
    expect_s3_class(res$diff_df, "data.frame")
  }
)

test_that("unsupported covariate types throw an error",
  {
    empty <- make_empty_diff("staggered", c("asian", "black", "male", "educ"))
    silo_data <- silo71
    silo_data$educ <- as.Date("2000-01-01") + seq_len(nrow(silo_data))
    expect_error(run_stage_two_71(empty, silo_data),
                 "Unsupported column type found for: educ")
  }
)

test_that("a lone single-category covariate falls back to no covariates",
  {
    empty <- make_empty_diff("staggered", "educ")
    silo_data <- silo71
    silo_data$educ <- "HS"
    expect_warning(res <- run_stage_two_71(empty, silo_data),
                   "only one category in this silo and were dropped: educ")
    expect_equal(res$diff_df$diff_estimate_covariates,
                 res$diff_df$diff_estimate)
  }
)

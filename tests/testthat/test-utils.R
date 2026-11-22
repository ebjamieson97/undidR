test_that(".ensure_full_rank() drops covariates collinear with the intercept",
  {
    # Dummies for every category (reference absent) sum to the intercept
    x <- cbind(1, c(0, 1, 0, 1, 1, 0),
               d1 = c(1, 0, 0, 1, 0, 0), d2 = c(0, 1, 1, 0, 1, 1))
    res <- .ensure_full_rank(x)
    expect_equal(ncol(res), 3)
    expect_equal(res[, 1:2], x[, 1:2])
    expect_equal(colnames(res)[3], "d1")

    # A constant covariate is collinear with the intercept
    x2 <- cbind(1, c(0, 1, 0, 1), c(2, 2, 2, 2), c(1, 3, 2, 5))
    expect_equal(.ensure_full_rank(x2), x2[, c(1, 2, 4)])

    # Already full rank: unchanged
    expect_equal(.ensure_full_rank(x2[, c(1, 2, 4)]), x2[, c(1, 2, 4)])
  }
)

test_that(".ensure_full_rank() with protect = FALSE",
  {
    x <- cbind(a = c(1, 1, 1), b = c(0, 0, 0), c = c(1, 2, 3),
               d = c(2, 3, 4))
    expect_equal(colnames(.ensure_full_rank(x, protect = FALSE)), c("a", "c"))

    # Zero-column input is returned as is
    x0 <- matrix(numeric(0), nrow = 3, ncol = 0)
    expect_equal(dim(.ensure_full_rank(x0, protect = FALSE)), c(3L, 0L))
  }
)

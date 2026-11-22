#' @keywords internal
#' Attempts to to regress y on x
.safe_solve <- function(x, y) {
  tryCatch({
    return(solve(t(x) %*% x) %*% t(x) %*% y)
  }, error = function(e) {
    return(NULL)
  })
}

#' @keywords internal
#' Compute beta coefficients and their SE's
.regress <- function(x, y, w = NULL, hc = "hc1") {

  # Make sure x is a matrix and y is a vector
  x <- as.matrix(x)
  y <- as.vector(y)

  # Check for and remove missing values
  x_valid <- complete.cases(x)
  y_valid <- !is.na(y)
  if (is.null(w)) {
    valid_mask <- x_valid & y_valid
  } else {
    w_valid <- !is.na(w)
    valid_mask <- x_valid & y_valid & w_valid
  }
  x <- as.matrix(x[valid_mask, ])
  y <- as.vector(y[valid_mask])

  # Make sure we have enough observations left
  ncolx <- ncol(x)
  n <- length(y)
  if (n < ncolx) {
    return(list(beta_hat = NA,
                beta_hat_se = NA,
                beta_hat_pval = NA))
  }

  # Computations without weights
  if (is.null(w)) {
    beta_hat <- .safe_solve(x, y)
    if (is.null(beta_hat)) {
      return(list(beta_hat = NA,
                  beta_hat_se = NA,
                  beta_hat_pval = NA))
    }
    if (n > ncolx) {
      resid <- y - (x %*% beta_hat)
      beta_hat_cov <- .compute_hc_covariance(x, resid, hc)
    }

    # Computations with weights
  } else {
    w <- w / sum(w)
    sw <- sqrt(w)
    xw <- x * sw
    yw <- y * sw
    beta_hat <- .safe_solve(xw, yw)
    if (is.null(beta_hat)) {
      return(list(beta_hat = NA,
                  beta_hat_se = NA,
                  beta_hat_pval = NA))
    }
    if (n > ncolx) {
      resid_w <- yw - (xw %*% beta_hat)
      beta_hat_cov <- .compute_hc_covariance(xw, resid_w, hc)
    }
  }

  # When the the beta_hat_cov was calculated, take the 2nd value
  # (treatment indicator) unless the ncols was 1 (for the aggregation
  # of all the subaggregate ATTs). Also extract the beta_hat value
  if (ncolx == 1) {
    extract <- 1
  } else {
    extract <- 2
  }
  beta_hat <- beta_hat[extract]
  if ((n > ncolx) && (!is.null(beta_hat_cov))) {
    beta_hat_se <- sqrt(diag(beta_hat_cov)[extract])
  } else {
    beta_hat_se <- NA
  }

  # Get pval if possible
  dof <- n - ncolx
  if (dof > 0 && !is.na(beta_hat_se)) {
    pval <- 2 * (1 - pt(abs(beta_hat / beta_hat_se), dof))
  } else {
    pval <- NA
  }

  return(list(beta_hat = beta_hat,
              beta_hat_se = beta_hat_se,
              beta_hat_pval = pval))
}


#' @keywords internal
#' Returns a HCCME of the specified type
.compute_hc_covariance <- function(x, resid, hc) {

  x <- as.matrix(x)
  resid <- as.vector(resid)

  n <- nrow(x)
  k <- ncol(x)
  XXinv <- tryCatch({
    solve(t(x) %*% x)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(XXinv)) {
    return(NULL)
  }

  # Compute hat matrix diagonal if needed for HC2/HC3/HC4
  if (hc %in% c("hc2", "hc3", "hc4")) {
    h <- sapply(1:n, function(i) {
      t(x[i, ]) %*% XXinv %*% x[i, ]
    })
  }

  # Construct omega based on HC type
  if (hc == "hc0") {
    omega_diag <- resid^2
  } else if (hc == "hc1") {
    omega_diag <- (n / (n - k)) * (resid^2)
  } else if (hc == "hc2") {
    omega_diag <- (resid^2) / (1 - h)
  } else if (hc == "hc3") {
    omega_diag <- (resid^2) / ((1 - h)^2)
  } else if (hc == "hc4") {
    h_bar <- mean(h)
    delta <- pmin(4, h / h_bar)
    omega_diag <- (resid^2) / ((1 - h)^delta)
  }

  # Sandwich estimator
  return(XXinv %*% crossprod(x, omega_diag * x) %*% XXinv)

}

#' @keywords internal
#' Checks that input is given as TRUE or FALSE
.validate_logical_args <- function(logical, arg_name) {
  if (!(identical(logical, TRUE) || identical(logical, FALSE))) {
    stop(paste(arg_name, "must be a single `TRUE` or `FALSE` value."))
  }
}

#' @keywords internal
#' Checks that matrix is full rank, useful for regression
#' Columns are added one at a time (left to right) and a column is kept only
#' if it increases the rank of the matrix built so far. Rank is always judged
#' against the *whole* matrix, so a covariate that is collinear with the
#' intercept or treatment indicator (e.g. a set of dummies that sums to one
#' because the reference category is absent from the subset) is also dropped.
#' If `protect = TRUE`, the first two columns (intercept and treatment) are
#' always kept and only covariate columns are ever dropped.
.ensure_full_rank <- function(x, protect = TRUE) {
  x <- as.matrix(x)
  ncolx <- ncol(x)
  if (ncolx == 0) {
    return(x)
  }
  n_protected <- if (protect) min(2, ncolx) else 0
  keep <- seq_len(n_protected)
  current_rank <- if (n_protected > 0) {
    qr(x[, keep, drop = FALSE])$rank
  } else {
    0
  }
  for (j in setdiff(seq_len(ncolx), keep)) {
    candidate <- c(keep, j)
    candidate_rank <- qr(x[, candidate, drop = FALSE])$rank
    if (candidate_rank > current_rank) {
      keep <- candidate
      current_rank <- candidate_rank
    }
  }
  return(x[, keep, drop = FALSE])
}
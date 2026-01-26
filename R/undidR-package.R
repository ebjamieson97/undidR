#' undidR: Difference-in-Differences with Unpoolable Data
#'
#' @description
#' Implements difference-in-differences with unpoolable data.
#'
#' @details
#' A framework for estimating difference-in-differences with unpoolable data,
#' based on Karim, Webb, Austin, and Strumpf (2025)
#' <https://doi.org/10.48550/arXiv.2403.15910>. Supports common or staggered
#' adoption, multiple groups, and the inclusion of covariates. Also computes
#' p-values for the aggregate average treatment effect on the treated via the
#' randomization inference procedure described in MacKinnon and Webb (2020)
#' <https://doi.org/10.1016/j.jeconom.2020.04.024>.
#'
#'
#' @section Stage One - Initialize:
#' \itemize{
#'   \item \code{\link{create_init_csv}} - Create initial CSV files for setup.
#'   \item \code{\link{create_diff_df}} - Creates the "diff matrix".
#' }
#'
#' @section Stage Two - Silos:
#' \itemize{
#'   \item \code{\link{undid_stage_two}} - Calculates trends and differences.
#' }
#'
#' @section Stage Three - Analysis:
#' \itemize{
#'   \item \code{\link{undid_stage_three}} - Computes ATTs and p-values.
#' }
#'
#' @author Eric Jamieson
#' Maintainer: <ericbrucejamieson@@gmail.com>
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
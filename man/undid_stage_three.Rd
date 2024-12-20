% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/undid_stage_three.r
\name{undid_stage_three}
\alias{undid_stage_three}
\title{Computes UNDID results}
\usage{
undid_stage_three(
  dir_path,
  agg = "silo",
  weights = TRUE,
  covariates = FALSE,
  interpolation = FALSE,
  save_csv = FALSE,
  filename = "UNDID_results.csv",
  filepath = tempdir(),
  nperm = 1001,
  verbose = TRUE
)
}
\arguments{
\item{dir_path}{A character specifying the filepath to the folder containing
all of the filled_diff_df_ csv files.}

\item{agg}{A character which specifies the aggregation methodology for
computing the aggregate ATT in the case of staggered adoption.
Either \code{"silo"}, \code{"g"}, or \code{"gt"}. Defaults to \code{"silo"}.}

\item{weights}{A logical value (either \code{TRUE} or \code{FALSE}) which determines
whether or not the weights should be used in the case of common adoption.
Defaults to \code{TRUE}.}

\item{covariates}{A logical value (either \code{TRUE} or \code{FALSE}) which specifies
whether to use the 'diff_estimate' or the 'diff_estimate_covariates'}

\item{interpolation}{A logical value or a string which specifies which,
if any, method of interpolation/extrapolation for missing values of
'diff_estimate' and 'diff_estimate_covariates' should be used.
Defaults to \code{FALSE}. There must be at least one diff_estimate or
'diff_estimate_covariates' value for the (silo,g) group for which
a missing value is being estimated in order for interpolation to work.
column when computing the aggregate ATT. Defaults to \code{FALSE}.}

\item{save_csv}{A logical value (either \code{TRUE} or \code{FALSE}) which determines
if a .csv copy of the UNDID results will be saved or not. Defaults to \code{FALSE}.}

\item{filename}{A string filename for the created .csv file.
Defaults to \code{UNDID_results.csv}}

\item{filepath}{Filepath to save the .csv file. Defaults to \code{tempdir()}.}

\item{nperm}{Number of random permutations of gvar & silo pairs to consider
when calculating the randomization inference p-value. Defaults to \code{1001}.}

\item{verbose}{A logical value (either \code{TRUE} or \code{FALSE}) which toggles
display output showing the progress of the randomization inference.
Defaults to \code{TRUE}.}
}
\value{
A dataframe of group level results (silo, g, or gt) as well
as the aggregate ATT and its standard erorrs and p-values from
two-sided tests of agg_ATT == 0.
}
\description{
Takes in all of the filled_diff_df_$silo_name.csv's
and uses them to compute the aggregate ATT, standard errors, and p-values.
}
\details{
The agg parameter specifies the aggregation method used in the
case of staggered adoption. By default it is set to "silo" so that the ATTs
are aggregated across silos with each silo having equal weight, but can be
set to "gt" or "g" instead. Aggregating across "g" calculates ATTs for
groups based on when the treatment time was, with each "g" group having equal
weight. Aggregating across "gt" calculates ATTs for groups based on when the
treatment time was and the time for which the ATT is calculated. This \code{agg}
parameter is ignored in the case of a common treatment time and only takes
effect in the case of staggered adoption. For common adoption, refer to the
\code{weights} parameter.
}
\examples{
NULL

}

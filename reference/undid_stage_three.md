# Computes UNDID results

Takes in all of the filled diff df CSV files and uses them to compute
group level ATTs as well as the aggregate ATT and its standard errors
and p-values.

## Usage

``` r
undid_stage_three(
  dir_path,
  agg = "g",
  weights = "both",
  covariates = FALSE,
  notyet = FALSE,
  nperm = 999,
  verbose = 100,
  check_anon_size = FALSE,
  hc = "hc3",
  only = NULL,
  omit = NULL,
  max_attempts = 100
)
```

## Arguments

- dir_path:

  A character specifying the filepath to the folder containing all of
  the filled diff df CSV files.

- agg:

  A character which specifies the aggregation methodology for computing
  the aggregate ATT in the case of staggered adoption. Options are:
  `"silo"`, `"g"`, `"gt"`, `"sgt"`, `"time"`, `"none"`. Defaults to
  `"g"`. `"silo"` computes a subaggregate ATT for each silo, `"g"`
  computes a subaggregate ATT for each unique treatment time, `"gt"`
  computes a subaggregate ATT for each unique treatment time &
  post-treatment period pair, `"sgt"` computes a subaggregate ATT for
  each treatment time & post-treatment pair, separated by silo, `"time"`
  computes subaggregate ATTs for grouped by time since treatment, and
  `"none"` does not compute any subaggregate ATTs, but rather computes
  an aggregate ATT directly from the differences.

- weights:

  A string, determines which of the weighting methodologies should be
  used. Options are: `"none"`, `"diff"`, `"att"`, or `"both"`. Defaults
  to the weighting choice specified in the filled diff CSV files.

- covariates:

  A logical value (either `TRUE` or `FALSE`) which specifies whether to
  use the `diff_estimate` column or the `diff_estimate_covariates`
  column from the filled diff df CSV files when computing ATTs.

- notyet:

  A logical value which declares if the not-yet-treated differences from
  treated silos should be used as controls when computing relevant
  sub-aggregate ATTs. Defaults to `FALSE`.

- nperm:

  Number of random permutations of treatment assignment to use when
  calculating the randomization inference p-value. Defaults to `999`.

- verbose:

  A numeric value (or `NULL`) which toggles messages showing the
  progress of the randomization inference once every `verbose`
  iterations. Defaults to `100`.

- check_anon_size:

  A logical value, which if `TRUE` displays which silos enabled the
  `anonymize_weights` argument in stage two, and their respective
  `anonymize_size` values. Defaults to `FALSE`.

- hc:

  Specify which heteroskedasticity-consistent covariance matrix
  estimator (HCCME) should be used. Options are `0`, `1`, `2`, `3`, and
  `4` (or `"hc0"`, `"hc1"`, `"hc2"`, `"hc3"`, `"hc4"`).

- only:

  A character vector of silos to include. Defaults to `NULL`.

- omit:

  A character vector of silos to omit. Defaults to `NULL`.

- max_attempts:

  A numeric value. Sets the maximum number of attempts to find a new
  unique random permutations during the randomization inference
  procedure. Defaults to `100`.

## Value

An UnDiDObj with S3 methods of
[`summary()`](https://rdrr.io/r/base/summary.html),
[`citation()`](https://rdrr.io/r/utils/citation.html),
[`print()`](https://rdrr.io/r/base/print.html), and
[`coef()`](https://rdrr.io/r/stats/coef.html).

## Examples

``` r
# Execute `undid_stage_three()`
dir <- system.file("extdata/staggered", package = "undidR")
undid_stage_three(dir, agg = "g", nperm = 399, verbose = NULL)
#> 
#>   Weighting: both
#>   Covariates: none
#>   Aggregation: g
#> 
#>   Aggregate ATT:  0.0761
#> 
#> (5 sub-aggregate estimates available via print(., level='sub'))
```

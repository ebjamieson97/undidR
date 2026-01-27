# Runs UNDID stage two procedures

Based on the information given in the received `empty_diff_df.csv`,
computes the appropriate differences in mean outcomes at the local silo
and saves as `filled_diff_df_$silo_name.csv`. Also stores trends data as
`trends_data_$silo_name.csv`.

## Usage

``` r
undid_stage_two(
  empty_diff_filepath,
  silo_name,
  silo_df,
  time_column,
  outcome_column,
  silo_date_format = NULL,
  consider_covariates = TRUE,
  filepath = tempdir(),
  anonymize_weights = FALSE,
  anonymize_size = 5
)
```

## Arguments

- empty_diff_filepath:

  A character filepath to the `empty_diff_df.csv`.

- silo_name:

  A character indicating the name of the local silo. Ensure spelling is
  the same as it is written in the `empty_diff_df.csv`.

- silo_df:

  A data frame of the local silo's data. Ensure any covariates are
  spelled the same in this data frame as they are in the
  `empty_diff_df.csv`.

- time_column:

  A character which indicates the name of the column in the `silo_df`
  which contains the date data. Ensure the `time_column` references a
  column of character values.

- outcome_column:

  A character which indicates the name of the column in the `silo_df`
  which contains the outcome of interest. Ensure the `outcome_column`
  references a column of numeric values.

- silo_date_format:

  A character which indicates the date format which the date strings in
  the `time_column` are written in.

- consider_covariates:

  An optional logical parameter which if set to `FALSE` ignores any of
  the computations involving the covariates. Defaults to `TRUE`.

- filepath:

  Character value indicating the filepath to save the CSV files.
  Defaults to [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- anonymize_weights:

  A logical value (defaults `FALSE`) which determines if the counts of n
  (# of obs. used to calculate a contrast/difference) and n_t (# of
  treated obs. used in the calculation of a contrast/difference) should
  be rounded.

- anonymize_size:

  A numeric value. Counts will be rounded to the nearest multiple of
  this value if `anonymize_weights` is `TRUE` (with a minimum value for
  any count being set as the value given for `anonymize_size`).

## Value

A list of data frames. The first being the filled differences data
frame, and the second being the trends data data frame. Use the suffix
\$diff_df to access the filled differences data frame, and use
\$trends_data to access the trends data data frame.

## Details

Covariates at the local silo should be renamed to match the spelling
used in the `empty_diff_df.csv`.

## Examples

``` r
# Load data
silo_data <- silo71
empty_diff_path <- system.file("extdata/staggered", "empty_diff_df.csv",
                               package = "undidR")

# Run `undid_stage_two()`
results <- undid_stage_two(
  empty_diff_filepath = empty_diff_path,
  silo_name = "71",
  silo_df = silo_data,
  time_column = "year",
  outcome_column = "coll",
  silo_date_format = "yyyy"
)
#> filled_diff_df_71.csv saved to: /tmp/Rtmp4vQlGn/filled_diff_df_71.csv
#> trends_data_71.csv saved to: /tmp/Rtmp4vQlGn/trends_data_71.csv

# View results
head(results$diff_df)
#>   silo_name gvar treat diff_times        gt RI start_time end_time weights
#> 1        71 1991     1  1991;1990 1991;1991  0       1989     2000    both
#> 2        71 1991     1  1992;1990 1991;1992  0       1989     2000    both
#> 3        71 1991     1  1993;1990 1991;1993  0       1989     2000    both
#> 4        71 1991     1  1994;1990 1991;1994  0       1989     2000    both
#> 5        71 1991     1  1995;1990 1991;1995  0       1989     2000    both
#> 6        71 1991     1  1996;1990 1991;1996  0       1989     2000    both
#>   diff_estimate    diff_var diff_estimate_covariates diff_var_covariates
#> 1    0.12916667 0.009447555              0.116348472         0.009397021
#> 2    0.06916667 0.008602222              0.069515594         0.008272557
#> 3    0.02546296 0.007975422              0.005133291         0.007767637
#> 4    0.02703901 0.008564103              0.029958108         0.008338060
#> 5    0.17361111 0.008686695              0.168621303         0.007994236
#> 6    0.13594633 0.008204221              0.146360101         0.007834932
#>         covariates date_format   freq   n n_t anonymize_size
#> 1 asian;black;male        yyyy 1 year  93  45             NA
#> 2 asian;black;male        yyyy 1 year  98  50             NA
#> 3 asian;black;male        yyyy 1 year 102  54             NA
#> 4 asian;black;male        yyyy 1 year  95  47             NA
#> 5 asian;black;male        yyyy 1 year 102  54             NA
#> 6 asian;black;male        yyyy 1 year 107  59             NA
head(results$trends_data)
#>   silo_name treatment_time time mean_outcome mean_outcome_residualized
#> 1        71           1991 1989    0.3061224                 0.1998800
#> 2        71           1991 1990    0.2708333                 0.1502040
#> 3        71           1991 1991    0.4000000                 0.1949109
#> 4        71           1991 1992    0.3400000                 0.1876636
#> 5        71           1991 1993    0.2962963                 0.1750943
#> 6        71           1991 1994    0.2978723                 0.1195425
#>         covariates date_format   freq  n
#> 1 asian;black;male        yyyy 1 year 49
#> 2 asian;black;male        yyyy 1 year 48
#> 3 asian;black;male        yyyy 1 year 45
#> 4 asian;black;male        yyyy 1 year 50
#> 5 asian;black;male        yyyy 1 year 54
#> 6 asian;black;male        yyyy 1 year 47

# Clean up temporary files
unlink(file.path(tempdir(), c("diff_df_71.csv",
                             "trends_data_71.csv")))
```

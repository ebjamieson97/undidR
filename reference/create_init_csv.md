# Creates the `init.csv`

The `create_init_csv()` function generates a CSV file with information
on each silo's start times, end times, and treatment times. If
parameters are left empty, generates a blank CSV with only the headers.

## Usage

``` r
create_init_csv(
  silo_names = character(),
  start_times = character(),
  end_times = character(),
  treatment_times = character(),
  covariates = character(),
  filename = "init.csv",
  filepath = tempdir()
)
```

## Arguments

- silo_names:

  A character vector of silo names.

- start_times:

  A character vector of start times.

- end_times:

  A character vector of end times.

- treatment_times:

  A character vector of treatment times.

- covariates:

  A character vector of covariates, or, `FALSE` (default).

- filename:

  A character filename for the created initializing CSV file. Defaults
  to `"init.csv"`.

- filepath:

  Filepath to save the CSV file. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

A data frame containing the contents written to the CSV file. The CSV
file is saved in the specified directory (or in a temporary directory by
default) with the default filename `init.csv`.

## Details

Ensure dates are entered consistently in the same date format. Call
[`undid_date_formats()`](https://ebjamieson97.github.io/undidR/reference/undid_date_formats.md)
to view valid date formats. Control silos should be marked as
`"control"` in the `treatment_times` vector. If `covariates` is `FALSE`,
no covariate column will be included in the CSV.

## Examples

``` r
create_init_csv(
  silo_names = c("73", "46", "54", "23", "86", "32",
                 "71", "58", "64", "59", "85", "57"),
  start_times = "1989",
  end_times = "2000",
  treatment_times = c(rep("control", 6),
                      "1991", "1993", "1996", "1997", "1997", "1998"),
  covariates = c("asian", "black", "male")
)
#> init.csv saved to: /tmp/RtmppI02hW/init.csv
#>    silo_name start_time end_time treatment_time       covariates
#> 1         73       1989     2000        control asian;black;male
#> 2         46       1989     2000        control asian;black;male
#> 3         54       1989     2000        control asian;black;male
#> 4         23       1989     2000        control asian;black;male
#> 5         86       1989     2000        control asian;black;male
#> 6         32       1989     2000        control asian;black;male
#> 7         71       1989     2000           1991 asian;black;male
#> 8         58       1989     2000           1993 asian;black;male
#> 9         64       1989     2000           1996 asian;black;male
#> 10        59       1989     2000           1997 asian;black;male
#> 11        85       1989     2000           1997 asian;black;male
#> 12        57       1989     2000           1998 asian;black;male
unlink(file.path(tempdir(), "init.csv"))
```

# Plot method for `UnDiDObj`

Plot method for `UnDiDObj`

## Usage

``` r
# S3 method for class 'UnDiDObj'
plot(x, event = FALSE, event_window = NULL, ci = 0.95, ...)
```

## Arguments

- x:

  A `UnDiDObj` object

- event:

  Logical. If `TRUE`, creates an event study plot. If `FALSE` (default),
  creates a parallel trends plot.

- event_window:

  Numeric vector of length 2 specifying the event window as c(start,
  end). Default is `NULL` (use all available periods).

- ci:

  Numeric between 0 and 1 specifying confidence level. Default is 0.95.

- ...:

  other arguments passed to plot

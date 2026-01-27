# Plot method for `UnDiDObj`

Plot method for `UnDiDObj`

## Usage

``` r
# S3 method for class 'UnDiDObj'
plot(
  x,
  event = FALSE,
  event_window = NULL,
  ci = 0.95,
  lwd = 1,
  legend = "topright",
  ...
)
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

- lwd:

  Linewidth arg passed to
  [`lines()`](https://rdrr.io/r/graphics/lines.html),
  [`abline()`](https://rdrr.io/r/graphics/abline.html), and
  [`segments()`](https://rdrr.io/r/graphics/segments.html). Defaults to
  `1`.

- legend:

  Keywords for indicating desired legend location. Defaults to
  `"topright"`. Other options include any of the keywords used as x in
  `legend(x, ...)`.

- ...:

  other arguments passed to plot

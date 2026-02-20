# Changelog

## undidR 3.0.2

- Computation of HCCMEs is now faster by using a crossproduct approach
  rather than constructing a full omega matrix.
- Changed the default HCCME from HC3 to HC1 (i.e., `hc = 1`).
- Fixed an issue where standard errors were returning `NA` for certain
  edge cases.

## undidR 3.0.1

CRAN release: 2026-02-16

- Added the `lwd` and `legend` parameters to the S3
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  `UnDiDObj` which controls line width and legend locations,
  respectively. Removed the default titles of plots.
- Also made other minor aesthetic changes to the default plots, such as
  adding horizontal bars to the ends of the event study plot confidence
  bands (like the letter I).
- Removed “none” as an aggregation option for staggered adoption.

## undidR 3.0.0

CRAN release: 2026-01-26

- Added new aggregation methods for staggered adoption: `"sgt"`,
  `"time"`, and `"none"`.

  - `"sgt"` computes subaggregate ATTs for each treatment-time and
    post-treatment period pair, further separated by silo.
  - `"time"` computes subaggregate ATTs grouped by time since treatment.
  - `"none"` skips subaggregate ATT computation and directly computes
    the aggregate ATT from the underlying differences.

- [`undid_stage_three()`](https://ebjamieson97.github.io/undidR/reference/undid_stage_three.md)
  now returns an object of class `UnDiDObj`, providing a unified S3
  interface.

  - Added S3 methods: [`print()`](https://rdrr.io/r/base/print.html),
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`coef()`](https://rdrr.io/r/stats/coef.html), and
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- Added support for event study plots via the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  `UnDiDObj` objects.

- Added randomization inference p-values for subaggregate ATTs.

- Fixed a miscalculation of jackknife standard errors and p-values.

- Expanded weighting options for ATT computation: `"none"`, `"diff"`,
  `"att"`, and `"both"`.

  - `"diff"` applies weights when computing subaggregate ATTs.
  - `"att"` applies weights when aggregating subaggregate ATTs.
  - `"both"` applies weighting at both stages.
  - `"none"` disables weighting entirely.

## undidR 2.0.0

CRAN release: 2025-05-31

- start_time and end_time columns are now stored as strings in the
  format specified by date_format column in the empty_diff_df CSV file.
- Fixed [`as.Date()`](https://rdrr.io/r/base/as.Date.html) calls in
  [`undid_stage_two()`](https://ebjamieson97.github.io/undidR/reference/undid_stage_two.md),
  [`undid_stage_three()`](https://ebjamieson97.github.io/undidR/reference/undid_stage_three.md),
  and `plot_parllel_trends()` to ensure compatability for R versions
  4.0.0 and onwards.

## undidR 1.0.2

CRAN release: 2025-04-27

- Made compatible for R versions 4.0.0 onwards.

## undidR 1.0.1

CRAN release: 2025-02-24

- Fixed the miscalculation of standard weights during stage two under
  common adoption scenarios.

## undidR 1.0.0

CRAN release: 2025-01-23

- Initial CRAN submission.
- Updated installation instructions.

## undidR 0.1.0

- Initial development version (not yet submitted to CRAN).
- Provides a framework for implementing difference-in-differences with
  unpoolable data.

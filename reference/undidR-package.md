# undidR: Difference-in-Differences with Unpoolable Data

Implements difference-in-differences with unpoolable data.

## Details

A framework for estimating difference-in-differences with unpoolable
data, based on Karim, Webb, Austin, and Strumpf (2025)
<https://doi.org/10.48550/arXiv.2403.15910>. Supports common or
staggered adoption, multiple groups, and the inclusion of covariates.
Also computes p-values for the aggregate average treatment effect on the
treated via the randomization inference procedure described in MacKinnon
and Webb (2020) <https://doi.org/10.1016/j.jeconom.2020.04.024>.

## Stage One - Initialize

- [`create_init_csv`](https://ebjamieson97.github.io/undidR/reference/create_init_csv.md) -
  Create initial CSV files for setup.

- [`create_diff_df`](https://ebjamieson97.github.io/undidR/reference/create_diff_df.md) -
  Creates the "diff matrix".

## Stage Two - Silos

- [`undid_stage_two`](https://ebjamieson97.github.io/undidR/reference/undid_stage_two.md) -
  Calculates trends and differences.

## Stage Three - Analysis

- [`undid_stage_three`](https://ebjamieson97.github.io/undidR/reference/undid_stage_three.md) -
  Computes ATTs and p-values.

## See also

Useful links:

- <https://github.com/ebjamieson97/undidR>

- <https://ebjamieson97.github.io/undidR/>

- Report bugs at <https://github.com/ebjamieson97/undidR/issues>

## Author

Eric Jamieson Maintainer: <ericbrucejamieson@gmail.com>

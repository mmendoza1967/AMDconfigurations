# Compute an Average Membership Degree (AMD) curve

For every candidate number of configurations, this function runs fuzzy
c-means from several random initializations. Random starts are
computational attempts to estimate the same solution, not data
replicates. Consequently, the fit with the smallest fuzzy c-means
objective among the converged starts supplies the membership matrix
evaluated by
[`amd_score()`](https://mmendoza1967.github.io/AMDconfigurations/reference/amd_score.md)
at each `c`.

## Usage

``` r
compute_amd_curve(
  data,
  its,
  nin,
  nsp,
  seeds = NULL,
  verbose = TRUE,
  plot_curve = FALSE,
  open_device = TRUE,
  scale_data = FALSE,
  iter_max = 100,
  m = 2,
  preselect_top_sd = NULL
)
```

## Arguments

- data:

  Numeric matrix or data frame. Rows are observations and columns are
  variables. Rows with missing values are removed with a warning and
  recorded in the returned row map.

- its:

  Positive integer number of random starts for every candidate `c`.

- nin:

  Minimum candidate number of configurations. Must be at least 2.

- nsp:

  Maximum candidate number of configurations.

- seeds:

  Optional positive integer vector or matrix. Vectors retain the
  historical start-major order; unused trailing values are ignored. If
  `NULL`, seeds are sampled from the caller's active random-number
  generator and advance its state. A matrix must have one row per start
  and uniquely named columns for every evaluated `c`; this form
  preserves the seeds of common candidates when the upper search limit
  is extended.

- verbose:

  Logical; print progress information when `TRUE`.

- plot_curve:

  Logical; plot the primary AMD curve when `TRUE`.

- open_device:

  Logical; when `plot_curve = TRUE`, open a new graphics device before
  plotting. The default is `TRUE` for compatibility with the public
  0.1.0 function. It has no effect when `plot_curve = FALSE`.

- scale_data:

  Logical; standardize variables before fitting.

- iter_max:

  Positive integer maximum number of fuzzy c-means iterations.

- m:

  Fuzziness parameter; must be greater than one.

- preselect_top_sd:

  Optional positive integer. If supplied, retain only the variables with
  the largest standard deviations before optional scaling. Retained
  variables are ordered by decreasing standard deviation; their original
  indices and names are recorded in `settings`.

## Value

An object of class `"amd_curve"`. `schema_version` identifies the public
object layout. The primary `curve` contains AMD values from the
minimum-objective converged fit at each `c`. Components `mean`, `max`,
and `raw` retain the historical restart summaries for diagnostics and
compatibility. These historical summaries include all starts, including
nonconverged starts, and are not the primary curve. `selected_fits`
contains the exact fitted memberships that produced the primary curve.
`complete_rows` and `omitted_rows` map the fitted observations back to
the input rows. The `settings` component records the input and retained
variables and the exact centering and scaling vectors used to construct
the fitted data. `manifest_id` links the curve to its process-level
reproducibility manifest. Additional public components are `c_peak`, its
historical alias `c_opt`, `peak_at_boundary`, the per-start
`objectives`, `iterations` and `converged` matrices, selected-start
diagnostics, the seed matrix, `coordinates` (the selected membership
matrix at `c_peak`), `backend`, and `selection_rule`. `selected_fit` is
the element of `selected_fits` corresponding to `c_peak`.

## Details

AMD itself is defined independently of fuzzy c-means. Fuzzy c-means is
the membership-estimation backend used by this function.

The returned `c_peak` (and its compatibility alias `c_opt`) is only the
numerical maximum of the evaluated curve. In particular, a maximum at
the boundary of the examined range is flagged and must not by itself be
treated as evidence for that number of groups. If several evaluated
values are exactly tied for the maximum, the smallest `c` is returned.

Vector seeds, including seeds generated when `seeds = NULL`, preserve
the historical start-major mapping and therefore depend on the number of
evaluated candidates. Use a named seed matrix when comparing nested
search ranges. Extra named columns are allowed and ignored when their
`c` is not in the current range. Duplicate seeds within one candidate
`c` reduce the number of unique random starts and generate a warning.

Every curve contains a `manifest_id` linking it to the process-level
record returned by
[`amd_reproducibility_manifest()`](https://mmendoza1967.github.io/AMDconfigurations/reference/amd_reproducibility_manifest.md).
Retrieve the manifest immediately after each curve and save one copy of
every distinct identifier. Curves sharing an identifier can share the
saved copy.

## References

Mendoza, M. and Araújo, M. B. (2022). Biogeography of bird and mammal
trophic structures. *Ecography*, 2022(5), e06289.
[doi:10.1111/ecog.06289](https://doi.org/10.1111/ecog.06289)

## Examples

``` r
set.seed(42)
X <- rbind(
  matrix(rnorm(40, mean = -2), ncol = 2),
  matrix(rnorm(40, mean = 2), ncol = 2)
)
curve <- compute_amd_curve(
  X, its = 2, nin = 2, nsp = 3, seeds = 1:4, verbose = FALSE
)
curve$curve
#>         2         3 
#> 0.4353950 0.5243304 
```

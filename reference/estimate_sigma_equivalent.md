# Estimate the sigma-equivalent compactness of a dataset

This function calibrates the observed AMD peak of real data against
synthetic datasets generated with varying levels of isotropic Gaussian
noise (\\\sigma\\). For each candidate \\\sigma\\, synthetic data are
generated with the same number of samples, dimensionality, and number of
clusters as the real data. The AMD peak of each synthetic dataset is
computed, and the *sigma-equivalent* value is defined as the \\\sigma\\
whose synthetic AMD peak best matches the real AMD peak (either by
interpolation or nearest match).

## Usage

``` r
estimate_sigma_equivalent(
  real_data,
  its,
  nin,
  nsp,
  k_opt = NULL,
  sigmas,
  iter_max = 20,
  make_plot = FALSE,
  return_plot = TRUE,
  quiet = TRUE,
  open_device_each = FALSE,
  device_width = 7,
  device_height = 5,
  cube_size = 100,
  method = c("interpolate", "nearest"),
  standardize = FALSE,
  seed_base = 7,
  plot_sigma_curves = FALSE
)
```

## Arguments

- real_data:

  A numeric matrix or data frame of samples × features.

- its:

  Number of random initialisations per AMD computation.

- nin:

  Minimum number of clusters to evaluate.

- nsp:

  Maximum number of clusters to evaluate.

- k_opt:

  Optional; the optimal number of clusters for the real data. If `NULL`,
  it is estimated internally.

- sigmas:

  Numeric vector of candidate \\\sigma\\ values to evaluate.

- iter_max:

  Maximum number of iterations for fuzzy c-means.

- make_plot:

  Logical; if `TRUE`, produce a comparative plot of \\\sigma\\ vs
  synthetic AMD peaks.

- return_plot:

  Logical; if `TRUE`, return the comparative plot object.

- quiet:

  Logical; suppress console output from synthetic data generation.

- open_device_each:

  Logical; if `TRUE`, open a new graphics device for each sigma-curve
  plot (when `plot_sigma_curves = TRUE`).

- device_width, device_height:

  Size of graphics device for sigma-curve plots.

- cube_size:

  Side length of the hypercube used to place synthetic centroids.

- method:

  Method for estimating sigma-equivalent: `"interpolate"` or
  `"nearest"`.

- standardize:

  Logical; if `TRUE`, standardise synthetic data.

- seed_base:

  Base seed for reproducibility.

- plot_sigma_curves:

  Logical; if `TRUE`, plot the AMD curve for each candidate \\\sigma\\.

## Value

A list containing:

- amd_real_peak:

  AMD peak of the real dataset.

- k_opt:

  Optimal number of clusters for the real data.

- table_sigma_amd:

  Data frame of \\\sigma\\ vs synthetic AMD peaks.

- sigma_equivalent:

  Interpolated sigma-equivalent value.

- sigma_eq:

  Nearest-match sigma on the explored grid.

- extrapolated:

  Logical; whether interpolation required extrapolation.

- plot_comparative:

  Comparative plot object (if requested).

- best_i:

  Index of best-matching sigma.

- best_sigma:

  Best-matching sigma value.

- best_res_syn:

  Full AMD results for the best synthetic dataset.

- best_df_curve:

  Data frame of the AMD curve for the best sigma.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
X <- matrix(rnorm(1000), nrow = 100, ncol = 10)
out <- estimate_sigma_equivalent(
  real_data = X,
  its = 5,
  nin = 2,
  nsp = 6,
  sigmas = seq(1, 10, by = 2)
)
out$sigma_equivalent
} # }
```

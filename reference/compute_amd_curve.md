# Compute the AMD curve across a range of cluster numbers

This function computes the Average Membership Deviation (AMD) curve for
fuzzy c-means clustering across a sequence of cluster numbers `k`. For
each `k`, multiple random initialisations are performed and the AMD
value is computed as:

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

  A numeric matrix or data frame of samples (rows) × features (columns).

- its:

  Number of random initialisations per value of `k`.

- nin:

  Minimum number of clusters to evaluate.

- nsp:

  Maximum number of clusters to evaluate.

- seeds:

  Optional numeric vector of seeds for deterministic behaviour. Must
  have length `its * (nsp - nin + 1)`. If `NULL`, random seeds are
  drawn.

- verbose:

  Logical; print progress messages.

- plot_curve:

  Logical; if `TRUE`, plot the AMD curve.

- open_device:

  Logical; if `TRUE`, open a new graphics device for the plot.

- scale_data:

  Logical; if `TRUE`, standardise features before clustering.

- iter_max:

  Maximum number of iterations for fuzzy c-means.

- m:

  Fuzziness parameter for fuzzy c-means (default 2).

- preselect_top_sd:

  Optional integer; if provided, only the top-SD features are retained
  before clustering (useful for very high-dimensional data).

## Value

A list with components:

- k_opt:

  The optimal number of clusters (maximising AMD peak).

- max:

  Vector of AMD peak values for each `k`.

- mean:

  Vector of mean AMD values across repetitions.

- raw:

  Matrix of AMD values (rows = repetitions, columns = `k`).

## Details

\$\$ \mathrm{AMD}(k) = \mathrm{mean}(\max_i u\_{i}) - 1/k \$\$

where \\u_i\\ is the membership vector of sample \\i\\. The optimal
number of clusters is selected as the `k` that maximises the AMD peak
across repetitions.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
X <- matrix(rnorm(2000), nrow = 100, ncol = 20)
res <- compute_amd_curve(X, its = 10, nin = 2, nsp = 6)
res$k_opt
} # }
```

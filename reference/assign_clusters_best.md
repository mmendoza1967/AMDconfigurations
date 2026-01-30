# Select the best fuzzy c-means partition across repeated initialisations

This function runs fuzzy c-means clustering
([`e1071::cmeans`](https://rdrr.io/pkg/e1071/man/cmeans.html))
repeatedly with different random seeds and selects the partition that
maximises an AMD-like objective:

## Usage

``` r
assign_clusters_best(
  data,
  opt_cluster,
  nreps = 10,
  m = 2,
  iter.max = 20,
  scale_data = FALSE,
  seeds = NULL,
  preselect_top_sd = NULL
)
```

## Arguments

- data:

  A numeric matrix or data frame of samples × features.

- opt_cluster:

  Integer; number of clusters to fit.

- nreps:

  Number of repeated initialisations.

- m:

  Fuzziness parameter for fuzzy c-means (default 2).

- iter.max:

  Maximum number of iterations for fuzzy c-means.

- scale_data:

  Logical; if `TRUE`, standardise features before clustering.

- seeds:

  Optional numeric vector of seeds for deterministic behaviour. Must
  have length `nreps`. If `NULL`, random seeds are drawn.

- preselect_top_sd:

  Optional integer; if provided, only the top-SD features are retained
  before clustering (useful for very high-dimensional data).

## Value

A list with components:

- cluster:

  Integer vector of cluster labels aligned to the original data. Rows
  with missing values receive `NA`.

- membership:

  Membership matrix from the best fuzzy c-means run.

- centers:

  Cluster centroids from the best run.

- Mpm:

  Best AMD-like objective value.

## Details

\$\$ \mathrm{Mpm} = \mathrm{mean}(\max_i u_i) - 1/k \$\$

where \\u_i\\ is the membership vector of sample \\i\\. The best
partition is returned, with cluster labels aligned to the original row
order of the input data (rows with missing values receive `NA`).

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
X <- matrix(rnorm(1000), nrow = 100, ncol = 10)
out <- assign_clusters_best(X, opt_cluster = 3, nreps = 20)
table(out$cluster)
} # }
```

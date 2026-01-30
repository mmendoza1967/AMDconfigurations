# Generate synthetic clustered samples with isotropic Gaussian noise

This function generates synthetic datasets composed of `n_clusters`
Gaussian clusters in `n_dim`-dimensional space. Cluster centroids are
placed uniformly inside a hypercube of side `cube_size`, and samples are
drawn with isotropic Gaussian noise of standard deviation `std_dev`.

## Usage

``` r
create_synthetic_samples(
  n_samples,
  n_clusters,
  std_dev,
  n_dim,
  cube_size = 100,
  standardize = FALSE,
  center = TRUE,
  scale. = TRUE
)
```

## Arguments

- n_samples:

  Total number of samples to generate.

- n_clusters:

  Number of clusters to simulate.

- std_dev:

  Standard deviation of the Gaussian noise around each centroid.

- n_dim:

  Number of dimensions (features).

- cube_size:

  Side length of the hypercube where centroids are placed.

- standardize:

  Logical; if `TRUE`, standardise the final dataset (mean 0, sd 1 per
  feature).

- center, scale.:

  Logical arguments passed to
  [`scale()`](https://rdrr.io/r/base/scale.html) if
  `standardize = TRUE`.

## Value

A data frame of size `n_samples × n_dim` containing the synthetic
samples.

## Details

The function is used internally to calibrate the compactness of real
data by matching its AMD peak against synthetic datasets with varying
noise levels.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
syn <- create_synthetic_samples(
  n_samples = 200,
  n_clusters = 4,
  std_dev = 5,
  n_dim = 10
)
head(syn)
} # }
```

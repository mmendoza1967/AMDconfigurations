# Compute Average Membership Degree from soft memberships

`amd_score()` defines Average Membership Degree (AMD) independently of
the algorithm used to estimate the memberships. Rows represent
observations, columns represent candidate configurations, entries are
non-negative, and each row sums to one.

## Usage

``` r
amd_score(membership, tolerance = sqrt(.Machine$double.eps))
```

## Arguments

- membership:

  Numeric matrix or data frame with observations in rows and
  configurations in columns.

- tolerance:

  Non-negative numerical tolerance used to check membership bounds and
  row sums.

## Value

One un-named numeric AMD value in the interval `[0, 1 - 1 / c]`, where
`c` is the number of membership columns.

## Details

For a membership matrix with `c` columns, AMD is the mean of the largest
membership in each row minus `1 / c`. The term `1 / c` is the value
attained by a completely uniform membership vector. It is not a
distribution-free expectation under random assignment.

A one-column membership matrix is mathematically valid and has AMD zero.
Curves used to compare alternative numbers of configurations
nevertheless begin at `c = 2`; see
[`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md).

## References

Mendoza, M. and Araújo, M. B. (2022). Biogeography of bird and mammal
trophic structures. *Ecography*, 2022(5), e06289.
[doi:10.1111/ecog.06289](https://doi.org/10.1111/ecog.06289)

## Examples

``` r
memberships <- rbind(
  c(0.90, 0.10),
  c(0.25, 0.75),
  c(0.50, 0.50)
)
amd_score(memberships)
#> [1] 0.2166667
```

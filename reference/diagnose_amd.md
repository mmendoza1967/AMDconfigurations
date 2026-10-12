# Describe an AMD curve and its stored partitions

Summarise the primary curve, memberships and hard assignments without
fitting new models, selecting a number of groups or changing the input.
The selected fits must be those saved by
[`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md).

## Usage

``` r
diagnose_amd(object, ambiguity_gap = 0.1)
```

## Arguments

- object:

  An `amd_curve` with a primary curve and a selected fit for every
  evaluated candidate. Legacy 0.1.0 objects are not sufficient.

- ambiguity_gap:

  A number in `[0, 1]`. An observation is counted as ambiguous when its
  largest membership minus its second-largest membership is strictly
  less than this value. The default, 0.1, is a descriptive convention,
  not a validated universal decision threshold.

## Value

An object of class `amd_diagnostics` with:

- candidates:

  One row per evaluated `c`, describing curve neighbours, winning
  memberships, normalised membership entropy, top-two gaps and hard and
  soft group-size balance. Boundary local maxima are `NA`, since one
  neighbour is missing. A local maximum here means strictly greater than
  both evaluated neighbours; plateaus are not classified as peaks.

- groups:

  One row per membership column at each `c`. Hard sizes use the stored
  winning assignment. Soft masses are sums of memberships, not of
  memberships raised to the fuzziness parameter. Summaries within a hard
  group are `NA` when that group is empty.

- transitions:

  ARI, geometric-mean normalised mutual information (NMI) and
  directional fragmentation between successive evaluated candidates.
  Fragmentation is the fraction outside each group's most frequent group
  in the other partition. `from_fragmentation` describes splitting of
  source groups; `to_fragmentation` describes mixing of source groups in
  destination groups. These are descriptive overlaps, not evidence of
  physical splitting, merging or biological stability.

- curve_summary:

  Numerical maximum, exact ties, boundary status and the evaluated
  range. These do not constitute a scientific choice of K.

- dictionary:

  Definitions and scales for every table column.

- unavailable:

  Diagnostics not computed by this operation and why.

- complete_rows, omitted_rows:

  The original observation row map.

- settings:

  Diagnostic conventions and provenance.

## Details

Normalised observation entropy is `-sum(u * log(u)) / log(c)`, with
`0 * log(0)` interpreted as zero. Size entropy applies the same formula
to group fractions. Quantiles use
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) with
`type = 8`. Exact top-membership ties are reported separately and the
stored assignment is retained; no new tie-breaking or random-number
generation takes place.

Transitions compare the same observations, not the numerical group
labels. ARI and NMI are invariant to relabelling. Identical single-group
partitions have ARI and NMI one even if their occupied membership
columns differ. NMI is zero when only one of the partitions has a single
occupied group.

This function uses memberships and assignments only. It does not
calculate sample-to-sample distances, silhouette, geometric compactness,
density, connectivity or resampling stability, and does not require the
original data. It does not impute missing descriptive values or call a
supervised reader. Its working arrays grow linearly with the number of
observations per candidate, plus contingency tables between occupied
groups; it does not construct an observation-by-observation distance
matrix.

## See also

[`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md),
[`assign_configurations()`](https://mmendoza1967.github.io/AMDconfigurations/reference/assign_configurations.md),
[`amd_score()`](https://mmendoza1967.github.io/AMDconfigurations/reference/amd_score.md)

## Examples

``` r
set.seed(42)
X <- rbind(matrix(rnorm(40, -2), ncol = 2),
           matrix(rnorm(40, 2), ncol = 2))
curve <- compute_amd_curve(X, its = 2, nin = 2, nsp = 3,
                           seeds = 1:4, iter_max = 1000, verbose = FALSE)
diagnostics <- diagnose_amd(curve)
diagnostics$candidates
#>   c       amd mean_winning_membership q10_winning_membership
#> 1 2 0.4353950               0.9353950              0.8357168
#> 2 3 0.5243304               0.8576638              0.6019939
#>   median_winning_membership mean_membership_entropy q90_membership_entropy
#> 1                 0.9561074               0.2826285              0.6416417
#> 2                 0.8999393               0.3701649              0.7165261
#>   mean_top_two_gap q10_top_two_gap ambiguous_fraction tied_winner_fraction
#> 1        0.8707900       0.6714337              0.025                    0
#> 2        0.7443122       0.2977249              0.025                    0
#>   minimum_hard_fraction maximum_hard_fraction hard_size_entropy
#> 1                  0.50                 0.500         1.0000000
#> 2                  0.25                 0.475         0.9604876
#>   empty_hard_groups minimum_soft_fraction maximum_soft_fraction
#> 1                 0             0.4892867             0.5107133
#> 2                 0             0.2642398             0.4608214
#>   soft_size_entropy delta_previous   drop_next at_boundary local_maximum
#> 1         0.9996688             NA -0.08893545        TRUE            NA
#> 2         0.9682197     0.08893545          NA        TRUE            NA
#>   at_numerical_maximum
#> 1                FALSE
#> 2                 TRUE
diagnostics$transitions
#>   from_c to_c c_step amd_change       ari       nmi from_fragmentation
#> 1      2    3      1 0.08893545 0.6666225 0.7125278              0.275
#>   to_fragmentation
#> 1            0.025
# Inspect the curve and diagnostics before making an explicit choice.
assigned <- assign_configurations(curve, c = 2)
```

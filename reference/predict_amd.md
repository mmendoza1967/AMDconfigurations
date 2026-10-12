# Propose a modal resolution from an AMD analysis

Optional assistance for interpreting a complete AMD curve. It computes
the frozen `c = 2,...,14` AMD analysis (unless a compatible curve is
supplied), derives curve, data, and candidate-partition descriptors, and
applies five externally loaded random forests. Reliability estimates the
chance that the emitted one-versus-several-modes decision, or the exact
proposed K, is correct within the assessed simulation domain. It is not
directly the probability that groups exist. The target is the
numerically resolved number of density modes used as the simulation
reference, not necessarily the number of generating components. AMD
itself does not estimate a density.

## Usage

``` r
predict_amd(
  data,
  predictor,
  curve = NULL,
  seed = 1L,
  allow_extrapolation = FALSE,
  verbose = TRUE,
  max_working_bytes = 512 * 1024^2
)
```

## Arguments

- data:

  Numeric matrix or data frame, with observations in rows.

- predictor:

  An object returned by
  [`load_amd_predictor()`](https://mmendoza1967.github.io/AMDconfigurations/reference/load_amd_predictor.md).

- curve:

  Optional compatible `amd_curve`. Supplying it avoids repeating the
  principal AMD fits; it is checked against `data` and the frozen model
  contract.

- seed:

  Positive integer controlling AMD starts, stability refits, and any
  large-data sampling used by descriptors. The caller's random-number
  state is preserved.

- allow_extrapolation:

  Logical. Away from the validated numerical grid, retain calibrated
  numerical values but label interpolation or extrapolation as
  unvalidated instead of suppressing them.

- verbose:

  Logical; report progress while computing a new curve.

- max_working_bytes:

  Positive finite working-memory guard for the exact descriptors
  (default 512 MiB). The estimate is not a guarantee of total process
  memory and excludes the externally loaded forests. The frozen
  global-distance summaries use a seeded sample of 2000 rows when N \>
  2000; partition distances remain exact. This distinction is recorded
  in the resource diagnostics. No additional approximation bypasses the
  guard.

## Value

An object of class `"amd_prediction"` containing the AMD curve, proposed
numerically resolved density-mode count `K`, separate structure and
exact-modal-count reliability estimates, candidate scores, five-seed
proposals, diagnostics, and applicability.

## Details

This function does not replace inspection of the AMD curve, does not
claim that its proposal is ground truth, and does not assign
observations. When `K > 1` is proposed, use
[`assign_configurations()`](https://mmendoza1967.github.io/AMDconfigurations/reference/assign_configurations.md)
with the returned curve and a scientifically justified `c`.

The corrected forests use the documented missing-left-pair convention.
Their unchanged historical calibrators were reassessed retrospectively
on the previously examined final bank, not on a new prospective
validation. Certainties are suppressed by default outside the numerical
validation grid N = 150 or 600, D = 2, 6 or 15. Matching this grid does
not certify the geometry of new data. The observed validation output
support was K=1:8, although the mechanical candidate range is 1:14.
Constant predictors dropped during training include the zero-variance
fraction (always zero) and the minimum covariance-rank fraction (always
one). A departure from those constants suppresses certainty, even on the
numeric grid. This safeguard does not certify all other aspects of
new-data geometry.

Supplying a compatible curve avoids its 260 principal fits. The
additional five computational restarts per c required by the reader are
still run; these are not a bootstrap or other sample-level stability
analysis. Curve checks establish row/dimension and objective consistency
with the supplied data, not a cryptographic identity of a separately
created data file.

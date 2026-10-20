# Propose a modal resolution from an AMD analysis

Optional assistance for interpreting a complete AMD curve. It computes
the frozen `c = 2,...,14` AMD analysis (unless a compatible curve is
supplied), derives curve, data, and candidate-partition descriptors, and
applies five externally loaded random forests. Conditional reference
values estimate correctness of the one-versus-several-modes decision and
exact K under the reference simulation regime. Reliability for a new
distribution remains unestablished; these values are not the probability
that groups exist. The target is the numerically resolved number of
density modes used as the simulation reference, not necessarily the
number of generating components. AMD itself does not estimate a density.

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

  Logical. Away from the validated numerical grid, retain conditional
  simulation-reference values in `reference_reliability`, labelled as
  unvalidated interpolation or extrapolation. This never establishes
  reliability for the analysed distribution and does not override
  feature or output-support guards.

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
exact-modal-count conditional reference estimates. `reliability` remains
`NA` because applicability to the analysed distribution has not been
established. `reference_reliability` contains the own-calibrator values
subject to applicability guards. `agreement` contains forest vote
fractions, not probabilities of correctness. `has_structure` is retained
as a compatibility alias for `proposed_multiple_modes`. The object
includes candidate scores, five-seed proposals, diagnostics, and
applicability.

## Details

This function does not replace inspection of the AMD curve, does not
claim that its proposal is ground truth, and does not assign
observations. When `K > 1` is proposed, use
[`assign_configurations()`](https://mmendoza1967.github.io/AMDconfigurations/reference/assign_configurations.md)
with the returned curve and a scientifically justified `c`.

The drop-37 forests omit 37 dimensional descriptors and use the
corrected missing-left-pair convention. Their own correctness
calibrators were fitted only on selection populations. A subsequent
fixed-reader prospective bank gave 68.51 percent exact-count accuracy
versus 70.91 percent mean reference confidence. A controlled
distribution challenge gave 60.53 versus 66.71 percent. Thus the
calibrated values cannot certify accuracy for a new distribution merely
because N and D match the simulation grid.

Continuous exact-K and one/several values remain available separately in
`reference_reliability`; no certainty threshold chooses or rejects K. By
default these reference values are suppressed outside N = 150 or 600, D
= 2, 6 or 15, assessed output support K=1:8, or assessed feature
conditions. The mechanical candidate range remains 1:14. Departures from
omitted training-constant predictors also suppress reference values.
Neither forest unanimity nor a large reference value establishes
distributional applicability. Count correctness does not validate FCM
assignments or the scientific meaning of empirical configurations. See
the installed `READER_EVIDENCE.txt` for evidence and limits.

Supplying a compatible curve avoids its 260 principal fits. The
additional five computational restarts per c required by the reader are
still run; these are not a bootstrap or other sample-level stability
analysis. Curve checks establish row/dimension and objective consistency
with the supplied data, not a cryptographic identity of a separately
created data file.

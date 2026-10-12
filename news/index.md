# Changelog

## AMDconfigurations 0.2.0

### Optional model-assisted interpretation and applicability

- The optional reader explicitly labels its target as the numerically
  resolved density-mode count used as the simulation reference, not the
  generating component count or a direct density measurement by AMD.

- Off-grid proposals themselves are labelled exploratory. Departures
  from omitted training-constant values suppress certainty, even when
  off-grid numerical values were explicitly requested. Predictions and
  models are unchanged.

- Resource diagnostics now distinguish exact partition distances from
  the frozen global-distance sample capped at 2000 rows. The prior
  blanket claim of exact distances was incorrect outside the assessed N
  grid.

- Bundle target, creation metadata and confidence-calibrator formulas
  are validated; unreachable legacy-loading branches were removed.

- Historical models, calibrators, feature-helper arithmetic, the AMD
  core and stored-partition diagnostics are unchanged. The same
  corrected-v2 bundle assembled for 0.2.0.9014 remains compatible.

- Added optional `load_amd_predictor(path, sha256)` and
  [`predict_amd()`](https://mmendoza1967.github.io/AMDconfigurations/reference/predict_amd.md)
  with external corrected-v2 forests. No model download or training
  occurs.

- Historical v1 bundles are rejected by the corrected extractor, not
  silently reinterpreted. Their frozen study artefacts remain unchanged
  outside this API.

- The corrected reader uses zero for a nonexistent left-pair margin. Its
  historical confidence calibrators were reassessed, without refitting,
  on the previously examined final bank; this is retrospective transfer
  evidence.

- Structure-decision reliability and exact-K reliability are reported
  separately. Numerical-grid membership does not certify the geometry of
  real data. Certainties are suppressed by default outside the assessed
  N/D grid, and for unsupported outputs or previously unseen
  descriptor-missingness patterns.

- Diagnostics have an explicit working-memory guard before fitting.
  Exact partition distances and the frozen global-distance sample are
  distinguished explicitly.

- The base curve, assignment and descriptive workflow still need no
  forests, ranger or digest. The published AMD definition is unchanged.

### Stored-partition diagnostics

- Added
  [`diagnose_amd()`](https://mmendoza1967.github.io/AMDconfigurations/reference/diagnose_amd.md)
  for descriptive, read-only summaries of a saved curve, its
  memberships, hard/soft group sizes and adjacent partition transitions.
- The returned dictionary defines every diagnostic and its scale.
  Missing neighbours and summaries of empty hard groups remain `NA`, not
  imputed.
- ARI and NMI are invariant to group labels, including single occupied
  groups. Directional overlaps distinguish fragmentation from source
  mixing.
- No new fits, resampling, sample distance matrices, automatic K
  decisions or calibrated certainties are produced. The original data
  are not required.
- The published AMD formula, primary fit selection and assignment are
  unchanged. The optional supervised reader is a separate operation with
  external models.

### AMD definition and curve construction

- AMD retains the published definition
  `mean(apply(membership, 1, max)) - 1 / c`. It is not normalised to
  `[0,1]`.
- [`amd_score()`](https://mmendoza1967.github.io/AMDconfigurations/reference/amd_score.md)
  is now an exported operation independent of the algorithm used to
  estimate memberships. It accepts the analytical `c = 1` case, whose
  AMD is zero.
- Operational AMD curves still begin at `c = 2`.
- At each candidate `c`, the primary curve point now comes from the
  converged fuzzy c-means fit with the smallest objective. Random starts
  are fitting attempts, not statistical replicates.
- Every selected fit is stored. The final assignment no longer comes
  from an additional unrecorded fuzzy c-means run.
- `c_peak` is the numerical maximum of the primary curve. The historical
  `c_opt` name remains as an alias but no longer represents an automatic
  scientific choice.

### Assignment and reproducibility

- The recommended workflow is `assign_configurations(curve, c)`, with
  `c` explicitly chosen after inspecting the curve and relevant
  evidence. It extracts the exact fit that produced the selected curve
  point.
- Direct fitting with `assign_configurations(data, c, ...)` remains
  available as a provisional compatibility interface.
- Supplied seeds are now actually applied and recorded. Named seed
  matrices preserve common fits when a candidate range is extended.
- Objectives, iterations, convergence, selected starts, preprocessing,
  omitted rows and a process-level reproducibility manifest are
  retained.
- Fits that reach `iter_max` are retained as diagnostics but cannot
  supply the selected curve point or standalone assignment. Candidate
  ranges now fail early when their upper value is not smaller than the
  complete sample size.
- New `amd_curve` and `amd_assignment` objects carry
  `schema_version = 2L`.

### Compatibility with saved 0.1.0 objects

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) recognise saved
  public 0.1.0 `amd_curve` objects. They warn that the minimum-objective
  primary curve cannot be reconstructed and use only the stored
  historical summaries. A legacy `c_opt` marker is drawn only with
  `type = "max"`, the series from which version 0.1.0 obtained it, and
  not on the stored mean.
- New curve objects retain the public 0.1.0 components `c_opt`, `max`,
  `mean`, `raw` and `coordinates`.
- Direct assignment objects retain `c_opt`, `cluster`, `membership`,
  `centers` and `objective`.
- The S3 plot methods use the active graphics device by default. The
  legacy `compute_amd_curve(..., plot_curve = TRUE)` interface retains
  its public default of opening a device; `open_device` has no effect
  when plotting is disabled.

### Intentional incompatible changes

- `estimate_sigma_equivalent()` and all sigma-equivalent classes and
  methods have been removed from the canonical package.
- The numerical values of a curve can differ from version 0.1.0 because
  that release did not apply its advertised seeds, selected the largest
  AMD across starts and then produced `coordinates` with an additional
  unrecorded fit.
- The default graph now shows the primary minimum-objective curve rather
  than the mean across starts. `type = "mean"` and `type = "max"` remain
  available as diagnostics.

### External model distribution

- Corrected-v2 forests are distributed separately from the package. The
  base curve, diagnostics and assignment workflow requires no model
  download, `ranger` or `digest`. Model loading requires an explicit
  trusted SHA-256.
- Historical v1 forests, calibrators and the local audit records remain
  unchanged. Corrected-reader confidence transfer is retrospective
  evidence, not a new prospective assessment or a claim of universal
  reliability.

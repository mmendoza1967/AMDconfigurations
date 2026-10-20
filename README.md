# AMDconfigurations

## Average Membership Degree and AMD curves

`AMDconfigurations` implements the computational core of Average Membership
Degree (AMD). The package separates four operations that should not be
confused:

1. `amd_score()` evaluates AMD from an existing soft-membership matrix.
2. `compute_amd_curve()` estimates one membership matrix for each candidate
   number of configurations and evaluates AMD on it.
3. The user inspects and justifies the relevant resolution from the complete
   curve.
4. `assign_configurations()` extracts the exact stored fit for that explicitly
   selected resolution.

AMD is defined independently of fuzzy c-means. Fuzzy c-means is the current
membership-estimation backend used to construct curves. At a specified resolution
and data representation, its groups are empirical configurations / clusters of
the fitted FCM partition. They need not coincide with generator components,
density modes or modal basins. Count correctness does not validate assignments
or the substantive meaning of those configurations.

Users moving from public version 0.1.0 should read
[inst/MIGRATION.md](inst/MIGRATION.md).
Saved 0.1.0 curve objects remain inspectable, but they do not contain enough
information to reconstruct the new minimum-objective primary curve.

## Installation

This directory prepares version 0.4.0 with the drop-37 reader. It is a local
release candidate and has not yet been published. Install this candidate
explicitly from its built source archive:

```r
install.packages("AMDconfigurations_0.4.0.tar.gz", repos = NULL, type = "source")
```

## Core example

The example below uses three generating components. We extract a fitted FCM
partition at c=3 to illustrate the API; this does not equate components, density
modes and empirical configurations. Real analyses must justify `c` from
the observed curve and supporting diagnostics.

```r
library(AMDconfigurations)

set.seed(8301)
X <- rbind(
  matrix(rnorm(120, mean = 0, sd = 0.7), ncol = 3),
  matrix(rnorm(120, mean = 4, sd = 0.7), ncol = 3),
  matrix(rnorm(120, mean = 8, sd = 0.7), ncol = 3)
)

candidate_c <- 2:6
starts <- 5L
seed_matrix <- outer(
  seq_len(starts),
  candidate_c,
  function(start, c) 100000L + 1000L * c + start
)
colnames(seed_matrix) <- as.character(candidate_c)

curve <- compute_amd_curve(
  X,
  its = starts,
  nin = min(candidate_c),
  nsp = max(candidate_c),
  seeds = seed_matrix,
  iter_max = 1000,
  verbose = FALSE
)

plot(curve)

# Explicit illustrative resolution; not an independently verified modal truth.
chosen_c <- 3L
assigned <- assign_configurations(curve, c = chosen_c)
```

`curve$c_peak` and its compatibility alias `curve$c_opt` are numerical
summaries, not automatic scientific decisions. A maximum at the right boundary
does not establish a number of groups. An internal peak must be interpreted in
the context of the subsequent curve, possible plateaus, stability and the
examined range.

## Curve and stored-partition diagnostics

Version 0.2.0 adds a descriptive operation:

```r
diagnostics <- diagnose_amd(curve)
diagnostics$candidates    # Curve neighbours and membership/size summaries
diagnostics$groups        # Per-group hard counts and soft membership masses
diagnostics$transitions   # Label-invariant comparisons of successive partitions
diagnostics$dictionary    # Definitions and scales
diagnostics$unavailable   # What this operation does not measure
```

This operation reads exactly the fits already stored in `curve`; it does not
fit them again or choose `K`. It needs no original data and constructs no
sample distance matrix. Winning membership, entropy and ambiguity describe
memberships, not density or geometric compactness. ARI and NMI describe
agreement between successive partitions, not stability under resampling.
The default top-two gap of 0.1 is an explicit descriptive convention, not a
universal threshold for structure. A strict local-maximum flag is likewise
only a numerical summary, not a rule for interpreting an entire curve.

## Optional drop-37 model-assisted reader

The base workflow does not require a model, `ranger` or `digest`. The optional
API accepts the external `amd_predictor_bundle_v3_drop37.rds` and its SHA-256.
The candidate model and matching `manifest.json` are supplied separately
alongside the source candidate, without an automatic download. The model is
about 49 MiB and is not part of the CRAN source package.

The five discovery-trained forests use 194 raw and 231 processed predictors,
excluding the 37 dimensional descriptors responsible for the tested common-unit
sensitivity. Their own exact-K and one/several correctness calibrators were
fitted on selection populations only.

```r
# Optional dependencies; install them only if using the reader.
# install.packages(c("ranger", "digest"))
bundle_path <- "path/to/amd_predictor_bundle_v3_drop37.rds"
bundle_sha256 <- "3407a5a55c30a70e150afaa16a52ee4f2c8d3ed265941f3af8c4cb9d0c99035f"
reader <- load_amd_predictor(bundle_path, sha256 = bundle_sha256)
result <- predict_amd(X, reader, seed = 123, verbose = FALSE)
plot(result)
result$proposed_k
result$reference_reliability  # conditional on reference simulations
result$agreement              # forest votes, not correctness probability
result$reliability            # applicability to this distribution is unestablished

# A compatible saved curve avoids repeating the principal fits:
# c=2:14, 20 starts, m=2, iter_max=2000, no scaling or preselection.
# result <- predict_amd(X, reader, curve = compatible_curve, seed = 123)
```

The reader still performs five computational restarts per c required by the
feature contract. These are not sample resampling. Numerical curve maximum,
assisted proposal and scientific choice remain separate. K targets the
numerically resolved density-mode count of the simulation reference, not
necessarily generator components; AMD itself does not estimate density.

The reader estimates the population density-mode count. `proposed_k` is its
proposal; `agreement` describes the five forest votes. Neither consensus nor the
numerical AMD maximum is an automatic scientific choice of resolution.

`reliability` is `NA` with status
`not_established_for_analysed_distribution`. Continuous own-calibrator values
remain in `reference_reliability`, separately for exact K and one/several.
These are estimates conditional on the reference simulations, not certified
probabilities for a new distribution. Matching N and D does not establish
distributional applicability. A later prospective bank showed an exact-count
confidence excess of 2.40 percentage points; a controlled distribution challenge
showed an excess of 6.18 points. Unanimous forests can share the same error.

Reference values are suppressed by default outside N={150,600}, D={2,6,15},
assessed output support K=1:8, or assessed missingness and training-constant
conditions. `allow_extrapolation=TRUE` exposes off-grid reference values only;
it never certifies applicability or overrides feature/output guards.
No confidence cutoff chooses K. Do not alter data to match the numerical grid.
See `system.file("READER_EVIDENCE.txt", package="AMDconfigurations")`.

Historical v1/v2 bundles remain reproducible with their matching historical
package versions and are rejected by the active drop-37 interface. Family-
exclusion readers are archived diagnostics and are not included in this bundle.

A working-memory estimate is checked before new fits; the default 512 MiB
descriptor limit excludes loaded forests and is not a total-memory guarantee.
Partition distances use all rows. Frozen global-distance summaries use a
seeded sample of 2000 rows when N exceeds 2000, with sample count and sampling
flags reported. No additional approximation bypasses the guard.

## Reproducibility

Every curve records its seeds, convergence diagnostics, retained variables,
scaling transformation and row map. It also contains a `manifest_id` linking
it to the process-level environment returned by:

```r
manifest <- amd_reproducibility_manifest()
identical(curve$manifest_id, manifest$manifest_id)
```

Immediately after each curve, retrieve `amd_reproducibility_manifest()` and
save one copy of every distinct `manifest_id`; curves sharing an identifier can
share that copy. This also covers the uncommon case in which `RNGkind()` changes
within one R process. Named seed matrices are recommended because they preserve
the seeds associated with each `(start, c)` pair when the candidate range is
extended.

## Interpretation limits

The computational core computes AMD and preserves fitted objects and
diagnostics. It does not provide a universal automatic rule for reading peaks,
plateaus or sustained declines. Whether AMD height or peak morphology measures
compactness, separation or configuration definition remains a research
question.

## Reference

Mendoza, M. & Araujo, M. B. (2022). Biogeography of bird and mammal trophic
structures. *Ecography*, 2022(5), e06289. doi:10.1111/ecog.06289.

## License

Package code: MIT License. The new drop-37 model bundle: CC BY 4.0, attributed
to Manuel Mendoza. Historical bundles retain their existing licences.
The transcriptomic example dataset retains its separately attributed CC BY
4.0 licence; see [inst/COPYRIGHTS](inst/COPYRIGHTS) and the dataset help.

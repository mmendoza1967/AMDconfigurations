# Assign observations to a user-selected number of configurations

The official workflow supplies an `amd_curve` object. The function then
extracts the exact fit that produced the AMD value at the explicitly
requested `c`; no new clustering is performed.

## Usage

``` r
assign_configurations(
  data,
  c,
  its = 50,
  iter_max = 100,
  m = 2,
  scale_data = FALSE,
  verbose = TRUE,
  seeds = NULL
)
```

## Arguments

- data:

  Preferably an object returned by
  [`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md).
  A numeric matrix or data frame with observations in rows and variables
  in columns is accepted only through the provisional compatibility
  mode.

- c:

  One integer greater than or equal to 2. The number of configurations
  to extract or fit. It must have been evaluated when `data` is an AMD
  curve.

- its:

  Positive integer number of random starts in independent fitting mode
  (default: 50). Ignored when extracting from an AMD curve.

- iter_max:

  Positive integer maximum number of fuzzy c-means iterations in
  independent fitting mode (default: 100). Ignored when extracting from
  an AMD curve.

- m:

  Fuzziness parameter greater than one in independent fitting mode
  (default: 2). Ignored when extracting from an AMD curve.

- scale_data:

  Logical; standardize variables before independent fitting. Ignored
  when extracting from an AMD curve.

- verbose:

  Logical; print progress information in independent fitting mode.
  Ignored when extracting from an AMD curve.

- seeds:

  Optional vector of positive integer seeds, one per random start in
  independent fitting mode. Supplied seeds make the result reproducible
  and do not alter the caller's random-number state. Ignored when
  extracting from an AMD curve. If `NULL`, seeds are sampled from the
  caller's active random-number generator and advance its state. Unused
  trailing seeds are ignored. Duplicate seeds reduce the number of
  unique starts and generate a warning.

## Value

A list of class `"amd_assignment"`. `schema_version` identifies the
public object layout. The object contains the selected `c` (also in the
compatibility alias `c_opt`), hard assignments, memberships, centers,
AMD and objective values, selection metadata, and a `starts` data frame
with the diagnostics for every random start. `source` distinguishes
exact extraction from an AMD curve from an independent fit. Components
`backend` and `selection_rule` identify how memberships were estimated
and how the retained fit was selected. `complete_rows` and
`omitted_rows` map fitted observations back to the input rows when
incomplete rows were removed. `settings` records the fitting and
preprocessing contract, while `manifest_id` links the object to
[`amd_reproducibility_manifest()`](https://mmendoza1967.github.io/AMDconfigurations/reference/amd_reproducibility_manifest.md).
In curve-extraction mode, `settings` retains the complete candidate
range used to create the curve; in independent fitting mode it records
the single fitted `c`. If converged starts have exactly tied minimum
objectives, the first start is selected. The selected fit is always
converged under the stated iteration rule.

## Details

A provisional compatibility mode accepts a numeric matrix or data frame
and independently estimates memberships with repeated fuzzy c-means
starts. It is not the package's recommended scientific workflow and may
be deprecated or removed in a future version.

The function never chooses `c` automatically. In particular, passing an
AMD curve still requires an explicit `c`, so the user can incorporate
the shape of the curve rather than being forced to accept its numerical
maximum.

Future automation may assist with interpreting the curve and proposing
`c`, but it must be validated against known truth and must not silently
replace inspection of the complete curve.

AMD is defined independently of fuzzy c-means. Fuzzy c-means is only the
membership-estimation backend used by the current implementation of
[`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md)
and by the provisional compatibility mode.

## Examples

``` r
# \donttest{
set.seed(1)
X <- rbind(
  matrix(rnorm(100, mean = -2), ncol = 2),
  matrix(rnorm(100, mean = 2), ncol = 2)
)
curve <- compute_amd_curve(
  X, its = 5, nin = 2, nsp = 4, seeds = 1:15, verbose = FALSE
)

# Exact fit underlying the selected point of the inspected curve
extracted <- assign_configurations(curve, c = 2)

# }
```

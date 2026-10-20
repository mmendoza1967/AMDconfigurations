# Load the optional drop-37 AMD reader

Loads the external model bundle used by
[`predict_amd()`](https://mmendoza1967.github.io/AMDconfigurations/reference/predict_amd.md).
The large fitted random forests are deliberately not embedded in the
base package. Loading verifies a caller-supplied SHA-256, validates the
model and preprocessing contracts, and never downloads or refits a
model. Historical v1/v2 bundles remain reproducible with their matching
historical package versions.

## Usage

``` r
load_amd_predictor(path, sha256)
```

## Arguments

- path:

  Path to an `amd_predictor_bundle_v3_drop37` RDS file.

- sha256:

  Expected 64-character SHA-256 from the matching bundle manifest. The
  optional package `digest` is required for this integrity check.

## Value

An object of class `"amd_predictor"`.

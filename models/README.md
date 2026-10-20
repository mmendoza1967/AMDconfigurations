# Drop-37 optional AMD reader

The model is `amd_predictor_bundle_v3_drop37.rds`, compatible with 0.4.0 and
the frozen 0.3.0 package. This directory contains its manifest, not the binary.
The package does not download models or train forests.

The file is 51,787,778 bytes. Its SHA-256 is
`3407a5a55c30a70e150afaa16a52ee4f2c8d3ed265941f3af8c4cb9d0c99035f`.

Five 1000-tree forests were trained on 1725 discovery populations spanning
all six families, using 194 raw and 231 processed predictors. Their own
exact-K and one/several calibrators were fitted on 1721 selection populations.
Family-exclusion models and calibrators are not used.

The target remains population density-mode count. The package reports the
proposal and forest agreement separately from conditional simulation-reference
values. Reliability for the analysed distribution is not established by matching
N,D or by unanimous votes. Existing feature, numeric and output guards apply to
reference values. No confidence cutoff or numerical AMD peak chooses K.

See `inst/READER_EVIDENCE.txt` for the completed common-unit, prospective and
distribution-challenge assessments. The frozen model metadata describes its
assembly evidence; later assessments are documented without changing its hash.
Empirical configurations refer to the fitted FCM partition, and are distinct
from density modes, modal basins and generating components.

Model: CC BY 4.0, Manuel Mendoza. Package code: MIT.
Historical v1/v2 models require their matching historical package versions.
Public publication status is recorded separately from local package checks.

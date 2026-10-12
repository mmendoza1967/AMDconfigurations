# Corrected-v2 optional AMD reader

This directory contains the model manifest, not the model binary. The matching
GitHub release provides `amd_predictor_bundle_v2.rds` as a separate asset. The
package itself never downloads models and the manual AMD workflow needs none.

Use the SHA-256 in `manifest.json` with `load_amd_predictor()`. Do not trust a
file only because its filename matches. The expected file is 86,675,904 bytes
and its SHA-256 is
`961f7ab814b41414f9342398726cac8058a65095d98dc18966c6e316b665c630`.

The bundle contains five already-trained corrected-boundary forests and the
unchanged historical confidence calibrators. It records assembly version
0.2.0.9014; that label is preserved rather than recompressing the frozen models.
Compatibility is tested with package release 0.2.0. The training feature
contract and model are versioned together; historical v1 study artefacts are
not inputs to this interface and remain unchanged in the study archive.

The target is a numerically resolved density-mode count in synthetic reference
populations, not the number of generating components. AMD itself does not
estimate a density. The corrected reader's certainty transfer was checked
retrospectively on the previously examined final bank; it is not new prospective
validation. Numeric grid membership does not certify the geometry of real data.
Unassessed outputs, missingness or changes in training-constant descriptors
also restrict certainty reporting. See the package help and migration guide.

Code and these synthetic-data model artefacts are distributed under the MIT
license in the repository. The source package's separately attributed example
datasets retain their stated licences.

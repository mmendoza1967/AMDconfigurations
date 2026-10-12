## Update from 0.1.0

This update preserves the published Average Membership Degree formula. It
separates fixed-membership evaluation, curve construction, descriptive
diagnostics, explicit stored-partition extraction and optional model assistance.

The primary curve now uses the minimum-objective converged fit at each c;
random starts are computational attempts, not statistical replicates. Seeds
are actually applied and recorded. Historical 0.1.0 curve objects remain
inspectable without fabricating information absent from those objects.
Sigma-equivalent has been intentionally removed. These compatibility changes
and the reasons that recomputed curves can differ are documented in
inst/MIGRATION.md and NEWS.md.

The optional model reader has no automatic download, startup or training
behaviour. Its external model file is not in the source tarball. The base
workflow does not require ranger or digest; these are Suggests. Reader
integration is optional during CRAN checks and is exercised separately against
the fixed-checksum model. The simulated domain and retrospective nature of
confidence transfer are explicitly disclosed.

The bundled datasets are byte-identical to version 0.1.0; documentation of
their actual dimensions and provenance has been corrected. The separately
attributed transcriptomic data retain their CC BY 4.0 licence (inst/COPYRIGHTS).

## Verification record

No reverse dependencies were found in the complete CRAN source-package index
on 2026-10-12 (Depends, Imports, LinkingTo, Suggests and Enhances checked).
Release verification is recorded separately; this file will be completed
with current-R checks before submission and is excluded from the tarball.

# Record the reproducibility environment for an AMD process

Creates a compact record of the software and random-number environment
used by AMD. The manifest is created once and then reused within the
current R process while its random-number generator remains unchanged. A
different process, including each parallel worker, or a change to
[`RNGkind()`](https://rdrr.io/r/base/Random.html) receives a different
manifest identifier.

## Usage

``` r
amd_reproducibility_manifest()
```

## Value

An object of class `"amd_reproducibility_manifest"` containing a schema
version, a unique environment identifier, its generation and UTC
creation time, the process identifier, R and package versions,
[`RNGkind()`](https://rdrr.io/r/base/Random.html), and
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html).

## Details

[`compute_amd_curve()`](https://mmendoza1967.github.io/AMDconfigurations/reference/compute_amd_curve.md)
records only the manifest identifier, rather than duplicating the
complete session information in every curve. Retrieve the manifest
immediately after each curve and save one copy of every distinct
`manifest_id`. Curves sharing an identifier can share the saved copy.

## Examples

``` r
manifest <- amd_reproducibility_manifest()
manifest$manifest_id
#> [1] "amd-20261012T162640393542Z-p6323-g1"
```

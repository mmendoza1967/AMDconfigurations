# Ecological dataset used in the example

The dataset `Fulldata` contains the ecological data used in the example
script `inst/examples/example_ecology.R`. It is included in the package
to provide a complete reproducible workflow for the ecological case
study.

## Usage

``` r
data("Fulldata")
```

## Format

A data frame with 6,619 rows and 34 columns. Columns `lat`, `lon`,
`Key`, `island`, `Clst`, and `NAME` identify and describe sampling
units; `IFd`, `GIn`, `HIn`, `IFr`, `Crn`, `Pln`, `Frg`, `Nct`, and `Grn`
are the nine trophic-structure variables used by the package example;
and `BIO1` to `BIO19` are bioclimatic variables. See the cited article
and its supporting information for the complete variable definitions and
data provenance.

## Source

Data prepared for Mendoza and Araújo (2022),
[doi:10.1111/ecog.06289](https://doi.org/10.1111/ecog.06289) .

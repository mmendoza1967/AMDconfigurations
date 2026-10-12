#' Reduced transcriptomic dataset (1000 variables)
#'
#' This dataset contains the 1000 variables with highest variance from the
#' original transcriptomic dataset used in the AMDconfigurations package.
#'
#' The full cleaned dataset (~58,000 variables) is available at Zenodo
#' (DOI: https://doi.org/10.5281/zenodo.18604443).
#'
#' @format A numeric data frame with 931 rows (samples) and 1,000 columns
#'   (gene-expression variables), ordered by decreasing variance in the full
#'   cleaned expression matrix.
#' @usage data("Transcdata_small")
#' @source Mendoza, M. and Medina, M. A. (2026), *High-dimensional
#'   transcriptomic dataset for the AMDconfigurations package*, Zenodo,
#'   \doi{10.5281/zenodo.18604443}, CC BY 4.0.
"Transcdata_small"

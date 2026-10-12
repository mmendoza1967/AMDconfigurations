.amd_curve_schema <- function(object) {
  if (!inherits(object, "amd_curve")) return("invalid")

  current_fields <- c(
    "curve", "mean", "max", "c_peak", "peak_at_boundary",
    "selected_objectives", "selected_starts", "converged"
  )
  if (!is.null(object$schema_version)) {
    if (identical(object$schema_version, 2L) &&
        all(current_fields %in% names(object))) {
      return("current")
    }
    return("invalid")
  }

  if (all(current_fields %in% names(object))) return("current")

  legacy_fields <- c("mean", "max", "c_opt")
  if (all(legacy_fields %in% names(object))) return("legacy_0.1")

  "invalid"
}

.amd_curve_candidates <- function(values) {
  candidates <- suppressWarnings(as.numeric(names(values)))
  if (length(candidates) != length(values) || anyNA(candidates)) {
    candidates <- seq_along(values)
  }
  candidates
}

.warn_legacy_amd_curve <- function(action) {
  warning(
    "legacy AMDconfigurations 0.1.0 amd_curve detected; ",
    "the minimum-objective primary curve cannot be reconstructed. ",
    action,
    call. = FALSE
  )
}

#' Print an AMD curve object
#'
#' @param x An object of class `"amd_curve"`.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#' @export
print.amd_curve <- function(x, ...) {
  if (!inherits(x, "amd_curve")) {
    stop("Object is not of class 'amd_curve'.", call. = FALSE)
  }

  schema <- .amd_curve_schema(x)
  if (schema == "invalid") {
    stop("Object does not contain a recognised AMD curve schema.",
         call. = FALSE)
  }

  if (schema == "legacy_0.1") {
    .warn_legacy_amd_curve(
      "Printing only the stored 0.1.0 restart summaries."
    )
    ks <- .amd_curve_candidates(x$mean)
    cat("AMD curve object (legacy schema 0.1.0)\n")
    cat("  Stored c_opt: ", x$c_opt, "\n", sep = "")
    cat("  Stored maximum AMD: ", sprintf("%.4f", max(x$max, na.rm = TRUE)),
        "\n", sep = "")
    cat("  Candidate range: ", min(ks), " - ", max(ks), "\n", sep = "")
    cat("  Primary minimum-objective curve: unavailable\n")
    return(invisible(x))
  }

  ks <- .amd_curve_candidates(x$curve)

  cat("AMD curve object\n")
  cat("  Numerical peak c: ", x$c_peak, "\n", sep = "")
  cat("  AMD at peak: ", sprintf("%.4f", max(x$curve)), "\n", sep = "")
  cat("  Candidate range: ", min(ks), " - ", max(ks), "\n", sep = "")
  cat("  Fit selection: minimum objective among converged starts\n")
  if (!is.null(x$schema_version)) {
    cat("  Object schema: ", x$schema_version, "\n", sep = "")
  }
  if (isTRUE(x$peak_at_boundary)) {
    cat("  Caution: the numerical peak is at the search boundary.\n")
  }

  invisible(x)
}

#' Summarise an AMD curve object
#'
#' @param object An object of class `"amd_curve"`.
#' @param ... Unused.
#'
#' @return Invisibly returns `object`.
#' @export
summary.amd_curve <- function(object, ...) {
  if (!inherits(object, "amd_curve")) {
    stop("Object is not of class 'amd_curve'.", call. = FALSE)
  }

  schema <- .amd_curve_schema(object)
  if (schema == "invalid") {
    stop("Object does not contain a recognised AMD curve schema.",
         call. = FALSE)
  }

  if (schema == "legacy_0.1") {
    .warn_legacy_amd_curve(
      "Summarising only the stored 0.1.0 restart summaries."
    )
    ks <- .amd_curve_candidates(object$mean)
    cat("Summary of AMD curve (legacy schema 0.1.0)\n")
    cat("------------------------------------------\n")
    cat("Stored c_opt: ", object$c_opt, "\n", sep = "")
    cat("Stored maximum AMD: ",
        sprintf("%.4f", max(object$max, na.rm = TRUE)), "\n", sep = "")
    cat("Candidate range: ", min(ks), " - ", max(ks), "\n", sep = "")
    cat("Primary minimum-objective curve: unavailable\n\n")
    print(data.frame(
      c = ks,
      mean = unname(object$mean),
      max = unname(object$max)
    ), row.names = FALSE)
    return(invisible(object))
  }

  ks <- .amd_curve_candidates(object$curve)

  cat("Summary of AMD curve\n")
  cat("--------------------\n")
  cat("Numerical peak c: ", object$c_peak, "\n", sep = "")
  cat("AMD at numerical peak: ", sprintf("%.4f", max(object$curve)),
      "\n", sep = "")
  cat("Candidate range: ", min(ks), " - ", max(ks), "\n", sep = "")
  cat("Fit selection: minimum objective among converged starts\n")
  if (!is.null(object$schema_version)) {
    cat("Object schema: ", object$schema_version, "\n", sep = "")
  }
  if (isTRUE(object$peak_at_boundary)) {
    cat("Caution: the numerical peak is at the search boundary and is not,\n")
    cat("by itself, evidence for that number of groups.\n")
  }
  cat("\n")

  df <- data.frame(
    c = ks,
    curve = unname(object$curve),
    mean = unname(object$mean),
    max = unname(object$max),
    selected_objective = unname(object$selected_objectives),
    selected_start = unname(object$selected_starts),
    converged_starts = colSums(object$converged)
  )
  print(df, row.names = FALSE)

  invisible(object)
}

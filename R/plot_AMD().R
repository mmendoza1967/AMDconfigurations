#' Plot an AMD curve
#'
#' Plots the primary AMD curve returned by `compute_amd_curve()`. Historical
#' restart summaries remain available as diagnostic alternatives.
#'
#' Objects saved by public version 0.1.0 can still be plotted from their stored
#' `mean` or `max` restart summaries. Because those objects do not contain the
#' minimum-objective primary curve, the method warns and does not attempt to
#' reconstruct it.
#'
#' @param amd_object An object returned by `compute_amd_curve()`.
#' @param type Curve to plot: `"curve"` (default) uses the minimum-objective
#'   converged fit at every `c`; `"mean"` and `"max"` show historical summaries
#'   across all random starts, including nonconverged starts. They are retained
#'   only for diagnostics and compatibility.
#' @param open_device Logical; open a new graphics device before plotting.
#'   The default uses the active device, as is conventional for S3 plot methods.
#' @param ... Additional graphical parameters passed to [graphics::plot()].
#'
#' @return Invisibly returns `NULL`.
#' @export
plot_AMD <- function(amd_object,
                     type = c("curve", "mean", "max"),
                     open_device = FALSE,
                     ...) {
  if (!inherits(amd_object, "amd_curve")) {
    stop("amd_object must be an amd_curve object.", call. = FALSE)
  }

  schema <- .amd_curve_schema(amd_object)
  type_was_missing <- missing(type)
  if (length(open_device) != 1L || is.na(open_device) ||
      !is.logical(open_device)) {
    stop("open_device must be TRUE or FALSE.", call. = FALSE)
  }

  if (schema == "invalid") {
    stop("amd_object does not contain a recognised AMD curve schema.",
         call. = FALSE)
  }

  if (schema == "legacy_0.1") {
    if (type_was_missing) type <- "mean"
    type <- match.arg(type, c("curve", "mean", "max"))
    if (type == "curve") {
      stop(
        "a legacy AMDconfigurations 0.1.0 object has no stored primary curve; use type = \"mean\" or type = \"max\".",
        call. = FALSE
      )
    }
    .warn_legacy_amd_curve(
      paste0("Plotting the stored ", type, " AMD restart summary.")
    )
    y <- amd_object[[type]]
    # Version 0.1.0 derived c_opt from the maximum-across-starts series, not
    # from its default mean plot. Mark it only when that matching series is
    # requested, so compatibility does not reproduce the old visual mismatch.
    peak_c <- if (type == "max") amd_object$c_opt else NA_integer_
  } else {
    type <- match.arg(type)
    y <- switch(
      type,
      curve = amd_object$curve,
      mean = amd_object$mean,
      max = amd_object$max
    )
    peak_c <- if (type == "curve") amd_object$c_peak else NA_integer_
  }

  ks <- .amd_curve_candidates(y)

  if (open_device) grDevices::dev.new()
  graphics::plot(
    ks,
    y,
    type = "b",
    pch = 16,
    xlab = "Number of configurations (c)",
    ylab = if (type == "curve") "AMD" else sprintf("AMD (%s)", type),
    ...
  )
  if (is.finite(peak_c)) {
    graphics::abline(v = peak_c, col = "blue", lty = 3, lwd = 2)
  }

  invisible(NULL)
}

#' Plot method for AMD curve objects
#'
#' @param x An object of class `"amd_curve"`.
#' @param type Curve to plot; see `plot_AMD()`.
#' @param open_device Logical; open a new graphics device before plotting.
#' @param ... Additional arguments passed to `plot_AMD()`.
#'
#' @return Invisibly returns `NULL`.
#' @export
plot.amd_curve <- function(x,
                           type = c("curve", "mean", "max"),
                           open_device = FALSE,
                           ...) {
  if (missing(type)) {
    return(plot_AMD(
      amd_object = x,
      open_device = open_device,
      ...
    ))
  }
  plot_AMD(
    amd_object = x,
    type = type,
    open_device = open_device,
    ...
  )
}

#' Compute the AMD curve across a range of cluster numbers
#'
#' This function computes the Average Membership Deviation (AMD) curve for
#' fuzzy c-means clustering across a sequence of cluster numbers \code{k}.
#' For each \code{k}, multiple random initialisations are performed and the
#' AMD value is computed as:
#'
#' \deqn{ \mathrm{AMD}(k) = \mathrm{mean}(\max_i u_{i}) - 1/k }
#'
#' where \eqn{u_i} is the membership vector of sample \eqn{i}.
#' The optimal number of clusters is selected as the \code{k} that maximises
#' the AMD peak across repetitions.
#'
#' @param data A numeric matrix or data frame of samples (rows) × features (columns).
#' @param its Number of random initialisations per value of \code{k}.
#' @param nin Minimum number of clusters to evaluate.
#' @param nsp Maximum number of clusters to evaluate.
#' @param seeds Optional numeric vector of seeds for deterministic behaviour.
#'   Must have length \code{its * (nsp - nin + 1)}. If \code{NULL}, random seeds are drawn.
#' @param verbose Logical; print progress messages.
#' @param plot_curve Logical; if \code{TRUE}, plot the AMD curve.
#' @param open_device Logical; if \code{TRUE}, open a new graphics device for the plot.
#' @param scale_data Logical; if \code{TRUE}, standardise features before clustering.
#' @param iter_max Maximum number of iterations for fuzzy c-means.
#' @param m Fuzziness parameter for fuzzy c-means (default 2).
#' @param preselect_top_sd Optional integer; if provided, only the top-SD features
#'   are retained before clustering (useful for very high-dimensional data).
#'
#' @return A list with components:
#' \describe{
#'   \item{k_opt}{The optimal number of clusters (maximising AMD peak).}
#'   \item{max}{Vector of AMD peak values for each \code{k}.}
#'   \item{mean}{Vector of mean AMD values across repetitions.}
#'   \item{raw}{Matrix of AMD values (rows = repetitions, columns = \code{k}).}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- matrix(rnorm(2000), nrow = 100, ncol = 20)
#' res <- compute_amd_curve(X, its = 10, nin = 2, nsp = 6)
#' res$k_opt
#' }
#'
#' @export
compute_amd_curve <- function(data, its, nin, nsp,
                              seeds = NULL,
                              verbose = TRUE,
                              plot_curve = FALSE,
                              open_device = TRUE,
                              scale_data = FALSE,
                              iter_max = 100,
                              m = 2,
                              preselect_top_sd = NULL) {

  # --- Input checks and preparation -----------------------------------------
  stopifnot(is.matrix(data) || is.data.frame(data))

  X <- as.matrix(data)
  X <- X[complete.cases(X), , drop = FALSE]

  # Optional SD-based feature preselection
  if (!is.null(preselect_top_sd) && ncol(X) > preselect_top_sd) {
    sds  <- apply(X, 2, sd, na.rm = TRUE)
    keep <- order(sds, decreasing = TRUE)[seq_len(preselect_top_sd)]
    X <- X[, keep, drop = FALSE]
  }

  # Optional feature-wise standardisation
  if (scale_data) X <- scale(X)

  ks <- nin:nsp
  P  <- length(ks)

  # Deterministic behaviour if seeds are supplied
  if (is.null(seeds)) {
    seeds <- sample.int(1e8, its * P)
  }
  stopifnot(length(seeds) >= its * P)

  # Matrix to store AMD values
  out <- matrix(NA_real_, nrow = its, ncol = P, dimnames = list(NULL, ks))

  # --- Main loop: repeated FCM for each k -----------------------------------
  for (rep in seq_len(its)) {
    if (verbose) cat("Iteration", rep, "of", its, "\n")

    for (j in seq_along(ks)) {
      k <- ks[j]

      if (k >= nrow(X)) {
        out[rep, j] <- NA_real_
        next
      }

      set.seed(seeds[(rep - 1) * P + j])

      cl <- e1071::cmeans(
        X,
        centers = k,
        iter.max = iter_max,
        verbose = FALSE,
        m = m
      )

      maxprob <- apply(cl$membership, 1, max)
      out[rep, j] <- mean(maxprob) - 1 / k
    }
  }

  # --- Summaries ------------------------------------------------------------
  amd_max  <- apply(out, 2, max,  na.rm = TRUE)
  amd_mean <- apply(out, 2, mean, na.rm = TRUE)

  # --- Optional plot --------------------------------------------------------
  if (plot_curve) {
    if (open_device) {
      if (.Platform$OS.type == "windows") windows() else dev.new()
    }
    plot(ks, amd_mean, type = "b", pch = 16,
         xlab = "Number of clusters (k)", ylab = "AMD (mean)")
    abline(v = ks[which.max(amd_max)], col = "blue", lty = 3)
  }

  # --- Output ---------------------------------------------------------------
  list(
    k_opt = ks[which.max(amd_max)],
    max   = amd_max,
    mean  = amd_mean,
    raw   = out
  )
}

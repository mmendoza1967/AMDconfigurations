#' Select the best fuzzy c-means partition across repeated initialisations
#'
#' This function runs fuzzy c-means clustering (\code{e1071::cmeans}) repeatedly
#' with different random seeds and selects the partition that maximises an
#' AMD-like objective:
#'
#' \deqn{ \mathrm{Mpm} = \mathrm{mean}(\max_i u_i) - 1/k }
#'
#' where \eqn{u_i} is the membership vector of sample \eqn{i}.
#' The best partition is returned, with cluster labels aligned to the original
#' row order of the input data (rows with missing values receive \code{NA}).
#'
#' @param data A numeric matrix or data frame of samples × features.
#' @param opt_cluster Integer; number of clusters to fit.
#' @param nreps Number of repeated initialisations.
#' @param m Fuzziness parameter for fuzzy c-means (default 2).
#' @param iter.max Maximum number of iterations for fuzzy c-means.
#' @param scale_data Logical; if \code{TRUE}, standardise features before clustering.
#' @param seeds Optional numeric vector of seeds for deterministic behaviour.
#'   Must have length \code{nreps}. If \code{NULL}, random seeds are drawn.
#' @param preselect_top_sd Optional integer; if provided, only the top-SD features
#'   are retained before clustering (useful for very high-dimensional data).
#'
#' @return A list with components:
#' \describe{
#'   \item{cluster}{Integer vector of cluster labels aligned to the original data.
#'     Rows with missing values receive \code{NA}.}
#'   \item{membership}{Membership matrix from the best fuzzy c-means run.}
#'   \item{centers}{Cluster centroids from the best run.}
#'   \item{Mpm}{Best AMD-like objective value.}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- matrix(rnorm(1000), nrow = 100, ncol = 10)
#' out <- assign_clusters_best(X, opt_cluster = 3, nreps = 20)
#' table(out$cluster)
#' }
#'
#' @export
assign_clusters_best <- function(data, opt_cluster, nreps = 10,
                                 m = 2, iter.max = 20,
                                 scale_data = FALSE,
                                 seeds = NULL,
                                 preselect_top_sd = NULL) {

  # --------------------------------------------------------------------------
  # 1. Input checks and preparation
  # --------------------------------------------------------------------------
  stopifnot(is.matrix(data) || is.data.frame(data))

  X <- as.matrix(data)

  # Track complete rows to restore NA positions later
  ok_rows <- complete.cases(X)
  X <- X[ok_rows, , drop = FALSE]

  # Remove zero-variance or non-finite-variance columns
  sds <- apply(X, 2, sd, na.rm = TRUE)
  X <- X[, sds > 0 & is.finite(sds), drop = FALSE]

  # Optional: preselect top-SD features
  if (!is.null(preselect_top_sd) && ncol(X) > preselect_top_sd) {
    ord <- order(apply(X, 2, sd, na.rm = TRUE), decreasing = TRUE)
    X <- X[, ord[seq_len(preselect_top_sd)], drop = FALSE]
  }

  # Optional: feature-wise standardisation
  if (scale_data) X <- scale(X)

  # Deterministic behaviour if seeds are supplied
  if (is.null(seeds)) seeds <- sample.int(1e8, nreps)
  stopifnot(length(seeds) >= nreps)

  # --------------------------------------------------------------------------
  # 2. Repeated FCM runs to select the best partition
  # --------------------------------------------------------------------------
  best_Mpm <- -Inf
  best <- NULL

  for (i in seq_len(nreps)) {
    set.seed(seeds[i])

    cl <- e1071::cmeans(
      X,
      centers = opt_cluster,
      iter.max = iter.max,
      verbose = FALSE,
      m = m
    )

    # AMD-like objective
    maxprob <- apply(cl$membership, 1, max)
    Mpm <- mean(maxprob) - 1 / opt_cluster

    if (Mpm > best_Mpm) {
      best_Mpm <- Mpm
      best <- list(
        cluster    = as.integer(cl$cluster),
        membership = cl$membership,
        centers    = cl$centers
      )
      cat("Repetition:", i, "Mpm =", signif(Mpm, 6), "\n")
    }
  }

  # --------------------------------------------------------------------------
  # 3. Restore cluster labels to original row order
  # --------------------------------------------------------------------------
  out_cluster <- rep(NA_integer_, nrow(data))
  out_cluster[ok_rows] <- best$cluster

  # --------------------------------------------------------------------------
  # 4. Output
  # --------------------------------------------------------------------------
  list(
    cluster    = out_cluster,
    membership = best$membership,
    centers    = best$centers,
    Mpm        = best_Mpm
  )
}

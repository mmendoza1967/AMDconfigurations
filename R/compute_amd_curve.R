#' Compute the Average Membership Degree (AMD) curve
#'
#' This function computes the Average Membership Degree (AMD) curve for
#' fuzzy c-means clustering across a sequence of cluster numbers. For each
#' value of \code{k}, the algorithm is run multiple times and AMD summarises
#' how sharply samples are assigned to clusters across iterations. The function
#' returns the AMD values (raw, mean, max) and the estimated optimal number of
#' configurations <i>c</i><sub>opt</sub>.
#'
#' AMD is computed as the mean maximum membership across samples minus \eqn{1/k},
#' which corrects for the expected value under random assignment.
#'
#' @param data A numeric matrix or data frame. Non-numeric columns should be
#'   removed beforehand.
#' @param its Number of AMD iterations to compute.
#' @param nin Minimum number of clusters to evaluate.
#' @param nsp Maximum number of clusters to evaluate.
#' @param seeds Optional vector of random seeds for reproducibility. If
#'   \code{NULL}, seeds are generated internally.
#' @param verbose Logical; if \code{TRUE}, print progress information.
#' @param plot_curve Logical; if \code{TRUE}, plot the AMD curve.
#' @param open_device Logical; if \code{TRUE}, open a new graphics device.
#'   computing AMD.
#' @param scale_data Logical; if \code{TRUE}, standardize the data before computing AMD.
#' @param iter_max Maximum number of iterations for the fuzzy c-means algorithm.
#' @param m Fuzziness parameter for fuzzy c-means (typically 1.5–2.5).
#' @param preselect_top_sd Optional integer. If not \code{NULL}, restricts AMD
#'   computation to the variables with highest standard deviation.
#'
#' @return An object of class \code{"amd_curve"} with components:
#'   \describe{
#'     \item{k_opt}{Estimated optimal number of configurations
#'       <i>c</i><sub>opt</sub>.}
#'     \item{max}{Vector of maximum AMD values across iterations for each \code{k}.}
#'     \item{mean}{Vector of mean AMD values across iterations for each \code{k}.}
#'     \item{raw}{Matrix of AMD values (iterations × \code{k}).}
#'     \item{coordinates}{Final membership matrix for \code{k} =
#'       <i>c</i><sub>opt</sub>.}
#'   }
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
                              preselect_top_sd = NULL)
{
  stopifnot(is.matrix(data) || is.data.frame(data))
  X <- as.matrix(data)
  X <- X[complete.cases(X), , drop = FALSE]

  if (!is.null(preselect_top_sd) && ncol(X) > preselect_top_sd) {
    sds  <- apply(X, 2, sd, na.rm = TRUE)
    keep <- order(sds, decreasing = TRUE)[seq_len(preselect_top_sd)]
    X <- X[, keep, drop = FALSE]
  }

  if (scale_data) X <- scale(X)

  ks <- nin:nsp
  P  <- length(ks)

  if (is.null(seeds)) seeds <- sample.int(1e8, its * P)
  stopifnot(length(seeds) >= its * P)

  out <- matrix(NA_real_, nrow = its, ncol = P, dimnames = list(NULL, ks))

  for (rep in seq_len(its)) {
    if (verbose) cat("Iteration", rep, "of", its, "\n")

    for (j in seq_along(ks)) {
      k <- ks[j]
      if (k >= nrow(X)) { out[rep, j] <- NA_real_; next }

#     set.seed(seeds[(rep - 1) * P + j])

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

  amd_max  <- apply(out, 2, max,  na.rm = TRUE)
  amd_mean <- apply(out, 2, mean, na.rm = TRUE)

  c_opt <- ks[which.max(amd_max)]

# set.seed(seeds[1])
  cl_final <- e1071::cmeans(
    X,
    centers = c_opt,
    iter.max = iter_max,
    verbose = FALSE,
    m = m
  )

  coordinates <- cl_final$membership

  if (plot_curve) {
    if (open_device) dev.new()
    plot(ks, amd_mean, type = "b", pch = 16,
         xlab = "Number of configurations (c)", ylab = "AMD (mean)")
    abline(v = c_opt, col = "blue", lty = 3)
  }

  out_list <- list(
    c_opt      = c_opt,
    max        = amd_max,
    mean       = amd_mean,
    raw        = out,
    coordinates = coordinates
  )

  class(out_list) <- "amd_curve"
  return(out_list)
}


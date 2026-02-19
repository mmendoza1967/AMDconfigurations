#' Fuzzy c-means clustering with multiple random initializations
#'
#' This function performs fuzzy c-means clustering on a dataset using multiple
#' random initializations. For each initialization, the algorithm is run using
#' \code{e1071::cmeans()}, and the solution with the smallest within-cluster
#' objective function is retained.
#'
#' The function is independent from the AMD workflow. It simply provides a
#' robust fuzzy clustering procedure by selecting the best solution across
#' several random seeds.
#'
#' @param data A numeric matrix or data frame. Rows are samples and columns are
#'   features.
#' @param c Integer. Number of clusters.
#' @param its Integer. Number of random initializations (default: 50).
#' @param iter_max Integer. Maximum number of iterations for the c-means
#'   algorithm (default: 100).
#' @param m Numeric. Fuzziness parameter (default: 2).
#' @param scale_data Logical. If TRUE, the data matrix is scaled before
#'   clustering.
#' @param verbose Logical. If TRUE, progress messages are printed.
#'
#' @return A list of class \code{"amd_assignment"} containing:
#'   \describe{
#'     \item{c_opt}{The number of clusters used.}
#'     \item{cluster}{A vector of hard cluster assignments (1..c).}
#'     \item{membership}{The fuzzy membership matrix.}
#'     \item{centers}{Cluster centers from the best solution.}
#'     \item{objective}{The minimum within-cluster objective value.}
#'   }
#'
#' @examples
#' \dontrun{
#' X <- matrix(rnorm(2000), ncol = 10)
#' res <- assign_configurations(X, c = 4)
#' table(res$cluster)
#' }
#'
#' @export
assign_configurations <- function(data,
                                  c,
                                  its = 50,
                                  iter_max = 100,
                                  m = 2,
                                  scale_data = FALSE,
                                  verbose = TRUE)
{
  stopifnot(is.matrix(data) || is.data.frame(data))
  X <- as.matrix(data)
  X <- X[complete.cases(X), , drop = FALSE]

  if (scale_data) X <- scale(X)

  if (verbose) {
    cat("Assigning configurations with c =", c, "\n")
    cat("Replications:", its, "\n")
  }

  seeds <- sample.int(1e8, its)

  best_obj <- Inf
  best_cl  <- NULL

  for (i in seq_len(its)) {
    if (verbose) cat("Iteration", i, "of", its, "\n")

  # set.seed(seeds[i])

    cl <- e1071::cmeans(
      X,
      centers  = c,
      iter.max = iter_max,
      verbose  = FALSE,
      m        = m
    )

    if (cl$withinerror < best_obj) {
      best_obj <- cl$withinerror
      best_cl  <- cl
    }
  }

  membership <- best_cl$membership
  cluster    <- apply(membership, 1, which.max)

  out <- list(
    c_opt      = c,
    cluster    = cluster,
    membership = membership,
    centers    = best_cl$centers,
    objective  = best_obj
  )

  class(out) <- "amd_assignment"
  return(out)
}

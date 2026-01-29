#' Generate synthetic clustered samples with isotropic Gaussian noise
#'
#' This function generates synthetic datasets composed of \code{n_clusters}
#' Gaussian clusters in \code{n_dim}-dimensional space. Cluster centroids are
#' placed uniformly inside a hypercube of side \code{cube_size}, and samples
#' are drawn with isotropic Gaussian noise of standard deviation \code{std_dev}.
#'
#' The function is used internally to calibrate the compactness of real data
#' by matching its AMD peak against synthetic datasets with varying noise levels.
#'
#' @param n_samples Total number of samples to generate.
#' @param n_clusters Number of clusters to simulate.
#' @param std_dev Standard deviation of the Gaussian noise around each centroid.
#' @param n_dim Number of dimensions (features).
#' @param cube_size Side length of the hypercube where centroids are placed.
#' @param standardize Logical; if \code{TRUE}, standardise the final dataset
#'   (mean 0, sd 1 per feature).
#' @param center,scale. Logical arguments passed to \code{scale()} if
#'   \code{standardize = TRUE}.
#'
#' @return A data frame of size \code{n_samples × n_dim} containing the
#'   synthetic samples.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' syn <- create_synthetic_samples(
#'   n_samples = 200,
#'   n_clusters = 4,
#'   std_dev = 5,
#'   n_dim = 10
#' )
#' head(syn)
#' }
#'
#' @export
create_synthetic_samples <- function(n_samples, n_clusters, std_dev, n_dim,
                                     cube_size = 100,
                                     standardize = FALSE,
                                     center = TRUE, scale. = TRUE) {

  # --------------------------------------------------------------------------
  # 1. Input validation
  # --------------------------------------------------------------------------
  stopifnot(
    n_samples >= n_clusters,
    n_clusters >= 1,
    std_dev >= 0,
    n_dim >= 1,
    cube_size > 0
  )

  # --------------------------------------------------------------------------
  # 2. Generate cluster centroids inside a hypercube
  # --------------------------------------------------------------------------
  centers <- matrix(
    runif(n_clusters * n_dim, 0.2 * cube_size, 0.8 * cube_size),
    nrow = n_clusters
  )

  # Number of samples per cluster
  samples_per_cluster <- floor(n_samples / n_clusters)

  # --------------------------------------------------------------------------
  # 3. Internal function to generate samples for one cluster
  # --------------------------------------------------------------------------
  gen_block <- function(i, n) {
    cov_factors <- runif(n_dim, 0.5, 1.5)

    matrix(
      rnorm(
        n * n_dim,
        mean = rep(centers[i, ], each = n),
        sd   = rep(std_dev * cov_factors, each = n)
      ),
      ncol = n_dim, byrow = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # 4. Generate samples for all clusters
  # --------------------------------------------------------------------------
  dat <- do.call(
    rbind,
    lapply(seq_len(n_clusters), function(i) gen_block(i, samples_per_cluster))
  )

  # Add remaining samples if needed
  remaining <- n_samples - nrow(dat)
  if (remaining > 0) {
    extras <- do.call(
      rbind,
      lapply(sample(seq_len(n_clusters), remaining, TRUE),
             function(i) gen_block(i, 1))
    )
    dat <- rbind(dat, extras)
  }

  colnames(dat) <- paste0("Dim", seq_len(n_dim))
  df <- as.data.frame(dat)

  # --------------------------------------------------------------------------
  # 5. Optional standardisation
  # --------------------------------------------------------------------------
  if (standardize) {
    mu  <- colMeans(df)
    sds <- apply(df, 2, sd)
    sds[!is.finite(sds) | sds == 0] <- 1

    df <- sweep(df, 2, mu, FUN = "-")
    df <- sweep(df, 2, sds, FUN = "/")
  }

  # --------------------------------------------------------------------------
  # 6. Verbose output
  # --------------------------------------------------------------------------
  cat(
    nrow(df), "samples were generated in", n_clusters, "clusters",
    "(cube side =", cube_size, ", target sd =", std_dev,
    ", standardize =", standardize, ")\n"
  )

  # --------------------------------------------------------------------------
  # 7. Output
  # --------------------------------------------------------------------------
  return(df)
}

curve_profile_cmeans <- function(scores) {
  force(scores)

  function(x, centers, iter.max, verbose, m, ...) {
    c_value <- as.integer(centers)
    score <- unname(scores[[as.character(c_value)]])
    if (length(score) != 1L || !is.finite(score)) {
      stop("test profile has no score for c = ", c_value)
    }

    largest_membership <- score + 1 / c_value
    other_membership <- (1 - largest_membership) / (c_value - 1)
    membership_row <- c(largest_membership,
                        rep(other_membership, c_value - 1L))
    membership <- matrix(
      rep(membership_row, each = nrow(x)),
      nrow = nrow(x),
      ncol = c_value
    )
    colnames(membership) <- as.character(seq_len(c_value))

    fitted_centers <- matrix(
      rep(colMeans(x), each = c_value),
      nrow = c_value,
      byrow = FALSE
    )

    list(
      cluster = max.col(membership, ties.method = "first"),
      centers = fitted_centers,
      membership = membership,
      withinerror = as.numeric(c_value),
      iter = 1L
    )
  }
}

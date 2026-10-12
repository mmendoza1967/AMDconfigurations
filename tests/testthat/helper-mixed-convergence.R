mixed_convergence_cmeans <- function() {
  call_number <- 0L

  function(x,
           centers,
           iter.max = 100,
           verbose = FALSE,
           m = 2,
           ...) {
    call_number <<- call_number + 1L
    if (call_number > 2L) {
      stop("the mixed-convergence fake expects exactly two starts")
    }

    c_value <- if (length(centers) == 1L) centers else nrow(centers)
    if (c_value != 2L) {
      stop("the mixed-convergence fake only supports c = 2")
    }

    first_membership <- ifelse(seq_len(nrow(x)) %% 2L == 0L, 0.9, 0.1)
    if (call_number == 2L) {
      first_membership <- ifelse(first_membership > 0.5, 0.6, 0.4)
    }
    membership <- cbind(first_membership, 1 - first_membership)

    list(
      cluster = max.col(membership, ties.method = "first"),
      membership = membership,
      centers = matrix(0, nrow = 2L, ncol = ncol(x)),
      withinerror = if (call_number == 1L) 1 else 5,
      iter = if (call_number == 1L) iter.max else 2L
    )
  }
}

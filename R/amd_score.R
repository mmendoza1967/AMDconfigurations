#' Compute Average Membership Degree from soft memberships
#'
#' `amd_score()` defines Average Membership Degree (AMD) independently of the
#' algorithm used to estimate the memberships. Rows represent observations,
#' columns represent candidate configurations, entries are non-negative, and
#' each row sums to one.
#'
#' For a membership matrix with `c` columns, AMD is the mean of the largest
#' membership in each row minus `1 / c`. The term `1 / c` is the value attained
#' by a completely uniform membership vector. It is not a distribution-free
#' expectation under random assignment.
#'
#' A one-column membership matrix is mathematically valid and has AMD zero.
#' Curves used to compare alternative numbers of configurations nevertheless
#' begin at `c = 2`; see `compute_amd_curve()`.
#'
#' @param membership Numeric matrix or data frame with observations in rows and
#'   configurations in columns.
#' @param tolerance Non-negative numerical tolerance used to check membership
#'   bounds and row sums.
#'
#' @return One un-named numeric AMD value in the interval
#'   `[0, 1 - 1 / c]`, where `c` is the number of membership columns.
#'
#' @references
#' Mendoza, M. and Araújo, M. B. (2022). Biogeography of bird and mammal
#' trophic structures. *Ecography*, 2022(5), e06289.
#' \doi{10.1111/ecog.06289}
#'
#' @examples
#' memberships <- rbind(
#'   c(0.90, 0.10),
#'   c(0.25, 0.75),
#'   c(0.50, 0.50)
#' )
#' amd_score(memberships)
#' @export
amd_score <- function(membership,
                      tolerance = sqrt(.Machine$double.eps)) {
  if (!is.matrix(membership) && !is.data.frame(membership)) {
    stop("membership must be a numeric matrix or data frame.", call. = FALSE)
  }

  U <- as.matrix(membership)
  if (!is.numeric(U)) {
    stop("membership must contain only numeric values.", call. = FALSE)
  }
  if (nrow(U) < 1L || ncol(U) < 1L) {
    stop("membership must have at least one row and one column.", call. = FALSE)
  }
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) {
    stop("tolerance must be one finite non-negative number.", call. = FALSE)
  }
  if (any(!is.finite(U))) {
    stop("membership contains non-finite values.", call. = FALSE)
  }
  if (any(U < -tolerance) || any(U > 1 + tolerance)) {
    stop("membership values must lie in [0, 1].", call. = FALSE)
  }

  row_totals <- rowSums(U)
  if (any(abs(row_totals - 1) > tolerance)) {
    stop("each membership row must sum to one.", call. = FALSE)
  }

  c_value <- ncol(U)
  unname(mean(apply(U, 1L, max)) - 1 / c_value)
}

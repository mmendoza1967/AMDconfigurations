.with_preserved_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(expr)
}

.validate_amd_seeds <- function(seeds, required) {
  if (!is.numeric(seeds) ||
      length(seeds) < required ||
      any(!is.finite(seeds[seq_len(required)])) ||
      any(seeds[seq_len(required)] < 1) ||
      any(seeds[seq_len(required)] != floor(seeds[seq_len(required)])) ||
      any(seeds[seq_len(required)] > .Machine$integer.max)) {
    stop(
      "seeds must contain enough positive integer values for every fit.",
      call. = FALSE
    )
  }

  as.integer(seeds[seq_len(required)])
}

.warn_duplicate_start_seeds <- function(seed_matrix) {
  duplicate_candidates <- colnames(seed_matrix)[vapply(
    seq_len(ncol(seed_matrix)),
    function(j) anyDuplicated(seed_matrix[, j]) > 0L,
    logical(1L)
  )]
  if (length(duplicate_candidates) > 0L) {
    warning(
      "duplicate seed values within candidate c = ",
      paste(duplicate_candidates, collapse = ", "),
      " reduce the number of unique random starts.",
      call. = FALSE
    )
  }
  invisible(seed_matrix)
}

.prepare_amd_curve_seeds <- function(seeds, its, ks) {
  c_names <- as.character(ks)
  n_fits <- its * length(ks)
  dim_names <- list(paste0("start_", seq_len(its)), c_names)

  if (is.null(seeds)) {
    values <- sample.int(.Machine$integer.max, n_fits)
    return(matrix(
      values,
      nrow = its,
      ncol = length(ks),
      byrow = TRUE,
      dimnames = dim_names
    ))
  }

  if (!is.matrix(seeds)) {
    values <- .validate_amd_seeds(seeds, n_fits)
    return(matrix(
      values,
      nrow = its,
      ncol = length(ks),
      byrow = TRUE,
      dimnames = dim_names
    ))
  }

  if (nrow(seeds) != its) {
    stop(
      "a seed matrix must have one row per random start (its).",
      call. = FALSE
    )
  }
  if (is.null(colnames(seeds)) || anyNA(colnames(seeds)) ||
      any(colnames(seeds) == "") || anyDuplicated(colnames(seeds))) {
    stop(
      "a seed matrix must have unique, non-empty column names identifying c.",
      call. = FALSE
    )
  }

  missing_c <- setdiff(c_names, colnames(seeds))
  if (length(missing_c) > 0L) {
    stop(
      "the seed matrix has no column for c = ",
      paste(missing_c, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  selected <- seeds[, c_names, drop = FALSE]
  values <- .validate_amd_seeds(as.vector(selected), n_fits)
  matrix(
    values,
    nrow = its,
    ncol = length(ks),
    byrow = FALSE,
    dimnames = dim_names
  )
}

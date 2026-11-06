#'Trimmed mean
#'
#'Mean computed after discaring the lowest and highest share of values.
#'
#'@param x Numeric vector.
#'@param prop Share trimmed on eachside, in (0, 0.5).
#'@export

trimmed_mean <- function(x, prop = 0.1) {
  if (prop < 0 || prop >= 0.5) {
    stop("`prop` must be in [0, 0.5].")
  }
  mean(x, trim = prop)
}

#' Huber location estimator
#'
#' Robust estimate of the center of a sample: close to the mean for ordinary values, close
#' to the median for extreme ones.
#'
#' @param x Numeric vector.
#'@param k Threshold (in robust standard deviations) beyond which a value is down-weighted.
#'@param tol Convergence tolerance.
#'@param max_iter Maximum number of iterations.
#'@export

huber_loc <- function(x, k = 1.345, tol = 1e-6, max_iter = 100) {
  mu <- stats::median(x)
  s <- stats::mad(x)
  if (s == 0) return(mu)
  for (i in seq_len(max_iter)) {
    r <- (x - mu) / s
    w <- pmin(1, k / abs(r))
    mu_new <- sum(w * x) / sum(w)
    if (abs(mu_new - mu) < tol) break
    mu <- mu_new
  }
  mu_new
}

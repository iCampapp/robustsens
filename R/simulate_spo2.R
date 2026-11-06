#' Simulate a healthy pulse oximeter signal.
#'
#' @param n Number of measurements.
#' @param mean True oxygens saturation.
#' @param sd Standard deviation of the measurement noise.
#' @return A numeric vector of lenght n.
#' @export

simulate_spo2 <- function(n, mean = 97, sd = 1) {
  pmin(stats::rnorm(n, mean = mean, sd = sd), 100)
}

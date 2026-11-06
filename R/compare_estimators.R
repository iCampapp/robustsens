#' Compare the bias of estimators under contamination
#'
#' For each proportion of artifacts, simulates many contaminated signals
#' and measures how far each estimatoris, on average, from the true value.
#'
#' @param estimators A named list of estimator functions.
#' @param proportions Shares of corrupted measurements to test.
#' @param n Number of measurements per signal.
#' @param true_value True oxygen saturation of the simulated patient.
#' @param value Value taken by the artifacts.
#' @param n_rep Number of simulated signals per proportion.
#' @return A data frame with columns `proportion`, `estimator`, `bias`.
#' @export

compare_estimators <- function(estimators = list(mean = mean,
                                                 median = stats::median,
                                                 trimmed_mean = trimmed_mean,
                                                 huber = huber_loc),
                               proportions = c(0, 0.01, 0.02, 0.05, 0.1, 0.2),
                               n = 100, true_value = 97, value = 60,
                               n_rep = 200) {
  if(is.null(names(estimators))) {
    stop("`estimators` must be a named list.")
  }
  rows <- lapply(proportions, function(p) {
    est <- vapply(seq_len(n_rep), function(i) {
      x <- contaminate(simulate_spo2(n, mean = true_value), p, value)
      vapply(estimators, function(f) f(x), numeric(1))
    }, numeric(length(estimators)))
    est <- matrix(est, nrow = length(estimators))
    data.frame(proportion = p,
               estimator = names(estimators),
               bias = rowMeans(est) - true_value)
  })
  do.call(rbind, rows)
}

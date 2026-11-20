#' Compare the bias of estimators under contamination
#'
#' For each proportion of artifacts, simulates many contaminated signals
#' and measures how far each estimator, on average, from the true value.
#'
#' @param estimators A named list of estimator functions.
#' @param proportions Shares of corrupted measurements to test.
#' @param n Number of measurements per signal.
#' @param true_value True oxygen saturation of the simulated patient.
#' @param value Value taken by the artifacts.
#' @param n_rep Number of simulated signals per proportion.
#' @return A data frame with columns `proportion`, `estimator`, `bias`.
#' @export

# Ici, on mesure le biais de chaque estimateur selon la part d'artefacts.
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

  # Pour chaque part d'artefacts...
  rows <- lapply(proportions, function(p) {
    # On repete l'experience n_rep fois
    est <- vapply(seq_len(n_rep), function(i) {
      # On simule un signal sain, on le contamine...
      x <- contaminate(simulate_spo2(n, mean = true_value), p, value)
      # Et on applique a chaque estimateur un signal contamine.
      vapply(estimators, function(f) f(x), numeric(1))
    }, numeric(length(estimators)))

    # On stocke les valeurs corrompues de chaque estimateur dans une matrice.
    est <- matrix(est, nrow = length(estimators))
    # on stocke dans un data frame.
    data.frame(proportion = p,
               estimator = names(estimators),
               # Moyenne des estimations de chaque estimateur moins la valeur saine
               bias = rowMeans(est) - true_value)
  })

  # On empile tout dans un tableau, car c'est le format que R attend pour tracer.
  do.call(rbind, rows)
}

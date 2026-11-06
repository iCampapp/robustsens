#' Contaminate a signal with artefacts
#'
#' Replaces a random proportion of measurements by a fixed outlier value, as when motion
#' corrupts a pulse oximeter reading.
#'
#' @param x Numric vector of measurements.
#' @param proportion Share of measurements to corrupt, between 0 and 1.
#' @param value Value taken by the corrupted measurements.
#' @return A numeric vector of the same lenght as x.
#' @export

#contaminate() prend le vecteur de valeurs vraies x, le contamine suivant une proportion,
#donc proportion*length(x) sont contaminees, el les valeurs contaminees = 60.
#sample() choisit ensuite au hasard les indices (dans x) qui prendront ces valeurs

contaminate <- function(x, proportion, value = 60) {
  if (proportion < 0 || proportion >1) {
    stop("`proportion` must be between 0 and 1.")
  }
  n_artifacts <- round(proportion * length(x))
  idx <- sample(seq_along(x), n_artifacts)
  x[idx] <- value
  x
}

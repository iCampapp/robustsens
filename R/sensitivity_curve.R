#'Empirical sensitivity curve of an estimator
#'
#'Measures how much one extra measurement, placed at each value of a grid, moves the
#'estimator.
#'
#'@param x Numeric vector of healthy measurements.
#'@param estimator A function taking a numeric vector and returning one number (for example
#' `mean`, `median`, `trimmed_mean`).
#' @param grid Values tested for the extra measurement.
#' @return A data frame with columns `x` (tested value) and `sensitivity`.
#' @export

sensitivity_curve <- function(x, estimator = mean, grid = seq(50, 110, length.out = 200)) {
  n <- length(x) + 1
  base <- estimator(x)

  #On multiplie par n pour eviter que l'effet d'une mesure diminue quand l'echantillon augmente
  sc <- vapply(grid, function(v) n * (estimator(c(x, v)) - base), numeric(1))
  data.frame(x = grid, sensitivity = sc)
}

# sensitivity_curve() est le coeur du package. Elle ajoute une seule mesure,
# en chaque valeur x, aux donnees saines, et regarde de combien l'estimateur
# bouge. C'est la fonction d'influence calculee sur un echantillon.
# Autrement dit, c'est la derivee de Gateaux avec un pas fini t = 1/n, a la
# place de la limite.1

#' Plot the sensitivity curves of several estimators
#'
#' @param x Numeric vector of healthy measurements.
#' @param estimators A named list of estimator functions.
#' @param grid Values tested for the extra measurement.
#' @param ylim Vertical range. If NULL, it fits all the curves.
#' @param main Plot title.
#' @return Invisibly, the list of sensitivity curves.
#' @export

# Les courbes de plot_sensitivity representent l'effet d'une seule mesure ajoutee.

# ylim c'est l'etendue sur l'axe des ordonnees. Pour un estimateur, y peut aller
# de -3 a 3. Pour un autre, de -100 a 100. Ca depend de l'estimateur.
# lapply renvoie des listes.

plot_sensitivity <- function(x,
                             estimators = list(mean = mean,
                                               median = stats::median,
                                               trimmed_mean = trimmed_mean,
                                               huber = huber_loc),
                             grid = seq(50, 110, length.out = 200),
                             ylim = NULL,
                             main = "Sensitivity curves") {
  if (is.null(names(estimators))) {
    stop("`estimators` must be a named list.")
  }
  curves <- lapply(estimators, function(est) sensitivity_curve(x, est, grid))
  # Ici, on affecte donc les valeurs de ylim. range(d$sensitivity) donne son min et son max.
  if(is.null(ylim)) {
    ylim <- range(vapply(curves, function(d) range(d$sensitivity), numeric(2)))
  }
  #cols est la liste de couleurs des graphes.
  # On sait qu'on a 04 estimateurs, donc 04 couleurs auraient suffit. Mais si un utilisateur
  #entre un autre estilateur et qu'il y a moins de couleurs, le nouveau graphe sera NA.
  cols <- rep_len(c("black", "red", "blue", "forestgreen", "orange", "purple"),
                  length(curves))

  # On dessine le repere avec graphic::plot. Il n'y a aucune courbe a l'interieur.
  graphics::plot(grid, rep(0, length(grid)), type = "n", ylim = ylim,
                 xlab = "Added measurement (SpO2 %)",
                 ylab = "Sensitivity", main = main)

  # Ligne en pointilles(lty = 3) gris a la hauteur 0.
  graphics::abline(h = 0, lty = 3, col = "grey60")

  # Graphe de sensibilte
  for (i in seq_along(curves)) {
    graphics::lines(curves[[i]]$x, curves[[i]]$sensitivity,
                    col = cols[i], lwd = 2)
  }

  # Legende
  graphics::legend("bottomright", legend = names(curves), col = cols,
                   lwd = 2, bty = "n")

  invisible(curves)
}

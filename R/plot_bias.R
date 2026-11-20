#' Plot the bias of estimators against the share of artifacts
#'
#' @param res Output of [compare_estimators()].
#' @param main plot title.
#' @return Invisibly, `res`.
#' @export

plot_bias <- function(res, main = "Bias under contamination") {
  # On releve la liste des estimateurs presents dans le tableau, sans repetition.
  names_est <- unique(res$estimator)
  # Une couleur par estimateur
  cols <- rep_len(c("black", "red", "blue", "darkgreen", "orange", "purple"),
                  length(names_est))

  # On ouvre un graphique vide: type = "n"
  graphics::plot(range(res$proportion), range(res$bias), type = "n",
                 xlab = "Share of artifacts", ylab = "Bias (SpO2 %)",
                 main = main)

  # On met la ligne en pointilles
  graphics::abline(h = 0, lty = 3, col = "grey60")
  # Pour chaque estimateur, on isole ses lignes dans un tableau, puis on trace sa courbe.
  for (i in seq_along(names_est)) {
    d <- res[res$estimator == names_est[i], ]
    graphics::lines(d$proportion, d$bias, col = cols[i], lwd = 2, type = "b") # type = "b" pour BOTH points and lines
  }
  graphics::legend("bottomleft", legend = names_est, col = cols,
                   lwd = 2, bty = "n") # bty = "n": boxtype= aucun cadre

  # On n'affiche pas res dans la console.
  invisible(res)
}

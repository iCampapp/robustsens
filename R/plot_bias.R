#' Plot the bias of estimators against the share of artifacts
#'
#' @param res Output of [compare_estimators()].
#' @param main plot title.
#' @return Invisibly, `res`.
#' @export

plot_bias <- function(res, main = "Bias under contamination") {
  names_est <- unique(res$estimator)
  cols <- rep_len(c("black", "red", "blue", "darkgreen", "orange", "purple"),
                  length(names_est))

  graphics::plot(range(res$proportion), range(res$bias), type = "n",
                 xlab = "Share of artifacts", ylab = "Bias (SpO2 %)",
                 main = main)
  graphics::abline(h = 0, lty = 3, col = "grey60")
  for (i in seq_along(names_est)) {
    d <- res[res$estimator == names_est[i], ]
    graphics::lines(d$proportion, d$bias, col = cols[i], lwd = 2, type = "b")
  }
  graphics::legend("bottomleft", legend = names_est, col = cols,
                   lwd = 2, bty = "n")
  invisible(res)
}

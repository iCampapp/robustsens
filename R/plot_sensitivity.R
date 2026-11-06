#' Plot the sensitivity curves of several estimators
#'
#' @param x Numeric vector of healthy measurements.
#' @param estimators A named list of estimator functions.
#' @param grid Values tested for the extra measurement.
#' @param ylim Vertical range. If NULL, it fits all the curves.
#' @param main Plot title.
#' @return Invisibly, the list of sensitivity curves.
#' @export
#'
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
  if(is.null(ylim)) {
    ylim <- range(vapply(curves, function(d) range(d$sensitivity), numeric(2)))
  }
  cols <- rep_len(c("black", "red", "blue", "forestgreen", "orange", "purple"),
                  length(curves))

  graphics::plot(grid, rep(0, length(grid)), type = "n", ylim = ylim,
                 xlab = "Added measurement (SpO2 %)",
                 ylab = "Sensitivity", main = main)
  graphics::abline(h = 0, lty = 3, col = "grey60")
  for (i in seq_along(curves)) {
    graphics::lines(curves[[i]]$x, curves[[i]]$sensitivity,
                    col = cols[i], lwd = 2)
  }
  graphics::legend("bottomright", legend = names(curves), col = cols,
                   lwd = 2, bty = "n")
  invisible(curves)
}

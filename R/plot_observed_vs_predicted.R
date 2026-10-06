#' Plot Observed vs Predicted Yield
#'
#' Generates an observed vs posterior mean predicted yield plot
#' with 95% credible intervals.
#'
#' @param predictions A data frame returned by \code{\link{predict_bayesre}},
#'   containing columns \code{Yield_obs}, \code{Yhat_mean}, \code{Yhat_lo},
#'   and \code{Yhat_hi}.
#' @param save Logical; if \code{TRUE} the plot is saved to \code{filename}
#'   as a PNG file. Default is \code{FALSE}.
#' @param filename Character string giving the file name (including extension)
#'   for the saved plot. If \code{NULL}, a temporary file is used.
#' @param width Plot width in inches. Default is \code{8}.
#' @param height Plot height in inches. Default is \code{6}.
#' @param dpi Resolution in dots per inch. Default is \code{300}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' # Small toy predictions; no Stan model is needed.
#' toy_predictions <- data.frame(
#'   Yield_obs = c(10, 12, 15, 17),
#'   Yhat_mean = c(10.2, 11.7, 14.6, 17.4),
#'   Yhat_lo = c(9, 10.5, 13.5, 16),
#'   Yhat_hi = c(11.5, 13, 15.8, 18.5)
#' )
#' p <- plot_observed_vs_predicted(toy_predictions)
#' stopifnot(inherits(p, "ggplot"))
#' plot_observed_vs_predicted(
#'   toy_predictions,
#'   save = TRUE,
#'   filename = tempfile(fileext = ".png")
#' )
#'
#' # A full Stan workflow is intentionally excluded from the automatic
#' # example run because model compilation/sampling can exceed 5 seconds.
#' \donttest{
#' data(bayesre_example)
#' fit <- fit_bayesre(bayesre_example, chains = 1, iter = 500, warmup = 200)
#' pred <- predict_bayesre(fit, bayesre_example)
#' plot_observed_vs_predicted(pred)
#' }

#' @export
plot_observed_vs_predicted <- function(predictions,
                                       save = FALSE,
                                       filename = NULL,
                                       width = 8,
                                       height = 6,
                                       dpi = 300) {

  # Check required columns
  required_cols <- c("Yield_obs", "Yhat_mean", "Yhat_lo", "Yhat_hi")
  if (!all(required_cols %in% names(predictions))) {
    stop("Input must be output from predict_bayesre().", call. = FALSE)
  }

  # Create plot
  p <- ggplot2::ggplot(
    predictions,
    ggplot2::aes(x = Yield_obs, y = Yhat_mean)
  ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = Yhat_lo, ymax = Yhat_hi),
      width = 0,
      alpha = 0.4
    ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = "red",
      linewidth = 1
    ) +
    ggplot2::labs(
      x = "Observed Yield",
      y = "Predicted Yield (Posterior Mean)",
      title = "Observed vs Predicted Yield"
    ) +
    ggplot2::theme_minimal(base_size = 14)

  # Save plot safely
  if (isTRUE(save)) {
    if (is.null(filename)) {
      filename <- tempfile(fileext = ".png")
    }

    ggplot2::ggsave(
      filename = filename,
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  return(p)
}


#' Compute Model Metrics
#'
#' Computes root mean squared error (RMSE) and root mean absolute percentage
#' error (RMAPE) from posterior mean predictions.
#'
#' @param predictions A data frame returned by \code{\link{predict_bayesre}}.
#'
#' @return A named list with two numeric elements:
#'   \describe{
#'     \item{RMSE}{Root mean squared error between observed and posterior mean
#'       predicted yield. Expressed in the same units as the yield variable.}
#'     \item{RMAPE}{Root mean absolute percentage error, expressed as a
#'       percentage. Computed as \eqn{100 \times \sqrt{\overline{|e_i / y_i|}}},
#'       where \eqn{e_i} is the prediction error and \eqn{y_i} is the observed
#'       yield for observation \eqn{i}.}
#'   }
#'
#' @examples
#' # Small deterministic toy predictions; runs in well under 5 seconds.
#' toy_predictions <- data.frame(
#'   Yield_obs = c(10, 12, 15),
#'   Yhat_mean = c(9.5, 12.5, 14),
#'   Yhat_lo = c(8, 11, 13),
#'   Yhat_hi = c(11, 14, 16)
#' )
#' metrics <- compute_metrics(toy_predictions)
#' stopifnot(is.list(metrics), all(c("RMSE", "RMAPE") %in% names(metrics)))
#'
#' # A full Stan workflow is intentionally excluded from the automatic
#' # example run because model compilation/sampling can exceed 5 seconds.
#' \donttest{
#' data(bayesre_example)
#' fit <- fit_bayesre(bayesre_example, chains = 1, iter = 500, warmup = 200)
#' pred <- predict_bayesre(fit, bayesre_example)
#' compute_metrics(pred)
#' }

#' @export
compute_metrics <- function(predictions) {

  rmse <- sqrt(mean((predictions$Yield_obs - predictions$Yhat_mean)^2))

  rmape <- 100 * sqrt(mean(abs(
    (predictions$Yield_obs - predictions$Yhat_mean) /
      predictions$Yield_obs
  )))

  list(RMSE = rmse, RMAPE = rmape)
}

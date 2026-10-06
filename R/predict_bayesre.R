#' Posterior Predictions
#'
#' Draws posterior predictions from a fitted \code{stanfit} object and
#' summarises them per observation.
#'
#' @param fit A \code{stanfit} object returned by \code{\link{fit_bayesre}}.
#' @param data The data frame used to fit the model, containing a \code{Yield}
#'   column.
#'
#' @return A data frame with one row per observation and four columns:
#'   \describe{
#'     \item{Yield_obs}{Observed yield values from \code{data$Yield}.}
#'     \item{Yhat_mean}{Posterior mean of the predicted yield.}
#'     \item{Yhat_lo}{2.5th percentile of the posterior predictive distribution
#'       (lower bound of the 95\% credible interval).}
#'     \item{Yhat_hi}{97.5th percentile of the posterior predictive distribution
#'       (upper bound of the 95\% credible interval).}
#'   }
#'
#' @examples
#' # Lightweight input example; no Stan model is needed for this check.
#' toy_data <- data.frame(
#'   Yield = c(10, 12, 14),
#'   Temp = c(25, 27, 29),
#'   Precip = c(100, 120, 140)
#' )
#' stopifnot(is.numeric(toy_data$Yield), nrow(toy_data) == 3)
#'
#' # A real prediction requires a stanfit object. Model compilation/sampling
#' # can exceed the 5-second example limit, so the complete workflow remains
#' # donttest.
#' \donttest{

#' @export
predict_bayesre <- function(fit, data) {

  post_samples <- rstan::extract(fit, pars = "y_pred", permuted = TRUE)
  y_pred_mat   <- post_samples$y_pred

  yhat_mean <- apply(y_pred_mat, 2, mean)
  yhat_lo   <- apply(y_pred_mat, 2, quantile, probs = 0.025)
  yhat_hi   <- apply(y_pred_mat, 2, quantile, probs = 0.975)

  data.frame(
    Yield_obs = data$Yield,
    Yhat_mean = yhat_mean,
    Yhat_lo   = yhat_lo,
    Yhat_hi   = yhat_hi
  )
}

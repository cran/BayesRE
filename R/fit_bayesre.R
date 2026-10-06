#' Fit Bayesian Regression Model
#'
#' Fits Bayesian linear regression using Stan.
#'
#' @param data Data frame containing Yield, Temp, Precip
#' @param chains Number of MCMC chains
#' @param iter Total iterations
#' @param warmup Warmup iterations
#' @return A \code{stanfit} object containing the posterior samples.
#'   Use \code{\link{predict_bayesre}} to obtain predictions,
#'   \code{\link{plot_diagnostics}} for MCMC trace plots, and
#'   \code{\link{plot_response_surface}} for a yield response heatmap.
#'
#' @examples
#' # Lightweight input check that runs automatically.
#' toy_data <- data.frame(
#'   Yield = c(10, 12, 14),
#'   Temp = c(25, 27, 29),
#'   Precip = c(100, 120, 140)
#' )
#' stopifnot(all(c("Yield", "Temp", "Precip") %in% names(toy_data)))
#'
#' # Stan compilation/sampling can exceed the 5-second example limit, so the
#' # complete fitting workflow remains a donttest example.
#' \donttest{

#' @export
fit_bayesre <- function(data, chains = 1, iter = 500, warmup = 200) {
  stopifnot(all(c("Yield", "Temp", "Precip") %in% names(data)))
  stan_data <- list(
    N       = nrow(data),
    yield   = as.numeric(data$Yield),
    temp    = as.numeric(data$Temp),
    precip  = as.numeric(data$Precip)
  )
  stan_file <- system.file("stan/bayesre_model.stan", package = "BayesRE")
  model <- rstan::stan_model(file = stan_file)
  fit <- rstan::sampling(
    model,
    data    = stan_data,
    chains  = chains,
    iter    = iter,
    warmup  = warmup,
    seed    = 1234,
    control = list(adapt_delta = 0.95)
  )
  return(fit)
}

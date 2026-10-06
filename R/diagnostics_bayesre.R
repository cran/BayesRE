#' Plot Diagnostics
#'
#' Generates MCMC trace plots for the model parameters ...
#'
#' @param fit A \code{stanfit} object returned by \code{\link{fit_bayesre}}.
#'
#' @return A \code{ggplot} object (the trace-plot grid) returned invisibly by
#'   \code{bayesplot::mcmc_trace()}. The plot is also printed to the active
#'   graphics device as a side effect.
#'
#' @importFrom bayesplot mcmc_trace
#'
#' @examples
#' # A diagnostic plot requires a fitted stanfit object. Creating one can
#' # exceed the 5-second example limit, so the full workflow remains donttest.
#' \donttest{

#' @export
plot_diagnostics <- function(fit) {
  bayesplot::mcmc_trace(
    as.array(fit),
    pars = c("alpha", "beta_temp", "beta_precip", "sigma")
  )
}

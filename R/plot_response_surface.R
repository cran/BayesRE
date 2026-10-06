#' @importFrom stats quantile
NULL

#' Plot Yield Response Heatmap
#'
#' Generates a posterior mean yield response surface over a grid of temperature
#' and precipitation values, using the posterior means of the intercept and
#' regression coefficients extracted from the fitted model.
#'
#' @param fit A \code{stanfit} object returned by \code{\link{fit_bayesre}}.
#' @param temp_range A numeric vector of length 2 giving the minimum and maximum
#'   temperature (degrees Celsius) for the prediction grid. Default is
#'   \code{c(20, 35)}.
#' @param precip_range A numeric vector of length 2 giving the minimum and
#'   maximum precipitation (mm) for the prediction grid. Default is
#'   \code{c(50, 200)}.
#' @param grid_size A positive integer controlling the resolution of the
#'   prediction grid. A value of \code{n} produces an \code{n x n} grid of
#'   temperature-precipitation combinations. Default is \code{50}.
#'
#' @return A \code{ggplot} object displaying a tile heatmap with:
#'   \itemize{
#'     \item Temperature (degrees Celsius) on the x-axis.
#'     \item Precipitation (mm) on the y-axis.
#'     \item Posterior mean predicted yield mapped to the fill colour using
#'       the viridis colour scale.
#'   }
#'   The predicted yield at each grid point is computed as
#'   \eqn{\hat{\alpha} + \hat{\beta}_{\text{temp}} \times T +
#'   \hat{\beta}_{\text{precip}} \times P}, where \eqn{\hat{\alpha}},
#'   \eqn{\hat{\beta}_{\text{temp}}}, and \eqn{\hat{\beta}_{\text{precip}}}
#'   are the posterior means of the respective parameters.
#'
#' @examples
#' # A response surface requires a fitted stanfit object. Stan compilation
#' # can exceed the 5-second example limit, so the full workflow remains
#' # donttest.
#' \donttest{

#' @export
plot_response_surface <- function(fit,
                                  temp_range   = c(20, 35),
                                  precip_range = c(50, 200),
                                  grid_size    = 50) {

  post <- rstan::extract(fit)

  alpha       <- mean(post$alpha)
  beta_temp   <- mean(post$beta_temp)
  beta_precip <- mean(post$beta_precip)

  temp_seq   <- seq(temp_range[1],   temp_range[2],   length.out = grid_size)
  precip_seq <- seq(precip_range[1], precip_range[2], length.out = grid_size)

  grid <- expand.grid(Temp   = temp_seq,
                      Precip = precip_seq)

  grid$Yield_pred <- alpha +
    beta_temp   * grid$Temp +
    beta_precip * grid$Precip

  p <- ggplot2::ggplot(grid,
                       ggplot2::aes(x    = Temp,
                                    y    = Precip,
                                    fill = Yield_pred)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(
      title = "Posterior Mean Yield Response Surface",
      x     = "Temperature (\u00B0C)",
      y     = "Precipitation (mm)",
      fill  = "Yield"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

#' Plot Response Surface with Observations
#'
#' Overlays observed temperature-precipitation data points on the posterior
#' mean yield response surface produced by \code{\link{plot_response_surface}}.
#'
#' @param fit A \code{stanfit} object returned by \code{\link{fit_bayesre}}.
#' @param data A data frame containing at least two columns: \code{Temp}
#'   (temperature in degrees Celsius) and \code{Precip} (precipitation in mm).
#'   These values are plotted as points on top of the heatmap.
#'
#' @return A \code{ggplot} object: the posterior mean yield heatmap from
#'   \code{\link{plot_response_surface}} with the observed
#'   temperature-precipitation points from \code{data} overlaid as filled
#'   black circles (size 1.5, alpha 0.7). The temperature and precipitation
#'   ranges of the heatmap default to \code{c(20, 35)} and \code{c(50, 200)}
#'   respectively; pass a customised base plot via
#'   \code{\link{plot_response_surface}} if different ranges are needed.
#'
#' @examples
#' # The overlay requires a fitted stanfit object and is therefore kept in
#' # donttest for automated checks.
#' \donttest{

#' @export
plot_response_surface_data <- function(fit, data) {

  p <- plot_response_surface(fit)

  p +
    ggplot2::geom_point(
      data         = data,
      mapping      = ggplot2::aes(x = Temp, y = Precip),
      inherit.aes  = FALSE,
      color        = "black",
      size         = 1.5,
      alpha        = 0.7
    )
}

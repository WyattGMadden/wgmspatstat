
#' Exponential Covariance Spatial Simulation
#' Given coordinates, phi, sigma squared, tau squared, a data matrix, and beta values,
#' a response with an exponential covariance can be simulated
#' @param lat latitudes of locations of interest
#' @param lon longitudes of locations of interest
#' @param phi decay parameter
#' @param sigma_sq measurement error
#' @param tau_sq nugget
#' @param X data matrix
#' @param beta coefficients
#' @keywords cats
#' @export
#' @examples
#' exp_cov_sim()
exp_cov_sim <- function(lat, lon, phi, sigma_sq, tau_sq, X, beta) {
  require(mnormt)
  dist_matrix <- as.matrix(dist(data.frame(x = lon, y = lat),
                                diag = T))
  
  cov_matrix <- sigma_sq * (exp(-phi * dist_matrix)) +
    (tau_sq + sigma_sq) * diag(nrow(X))
  
  return(rmnorm(n = 1,
                mean = X %*% beta,
                varcov = cov_matrix))
}

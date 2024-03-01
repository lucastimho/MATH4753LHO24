#' Normal Curve
#'
#' @param q quantile
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return plot and probability of density curved
#' @export
#'
#' @examples
myncurve <- function(q, mu=0, sigma=1){
  x = seq(-1e99, 1e99, length = 1000)
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  qcurve = seq(-1e99, q, length=1000)
  ycurve = dnorm(qcurve, mean=mu, sd=sigma)
  polygon(c(-1e99, qcurve, q), c(0, ycurve, 0), col="Red")
  area = round(pnorm(q, mean=mu, sd=sigma), 4)
  print(area)
}

#' Calculates the number of tickets to be sold when given the number of seats, probability of attendance, and probability of overbooking
#'
#' @param N number of seats
#' @param gamma probability of overbooking
#' @param p probability of attendence
#'
#' @return two plots of discrete distribution and continous distibution
#' @export
#'
#' @examples ntickets(N = 400, gamma = 0.02, p = 0.05)
ntickets <- function(N, gamma, p) {

  # Calculating nd using the binomial distribution
  objective <- function(n) {
    return(abs(N - qbinom(1 - gamma, n, p)))
  }
  nd <- round(optimise(objective, interval = c(N, N * 1.1), maximum = FALSE)$minimum)

  # Plotting discrete objective function vs n
  objective_discrete <- function(n) {
    return(N - qbinom(1 - gamma, n, p))
  }
  plot(seq(N, N * 1.1, by = 1), objective_discrete(seq(N, N * 1.1, by = 1)), type='b', col="blue", xlab="n", ylab="Objective", main=paste("Objective vs n to find optimal tickets sold \n(", nd, ") gamma =", gamma, "N =", N, "discete"))
  abline(h = 0, col = 'red', lty = 2)
  abline(v = nd, col= 'red', lty = 2)

  # Calculating nc using the normal approximation
  objective <- function(n) {
    return(abs(N - qnorm(1 - gamma, mean = n * p, sd = sqrt(n * p * (1 - p)))))
  }
  nc <- optimise(objective, interval = c(N, N * 1.1), maximum = FALSE)$minimum

  # Plotting continous objective function vs n
  objective_continuous <- function(n) {
    return(N - qnorm(1 - gamma, mean = n * p, sd = sqrt(n * p * (1 - p))))
  }
  plot(seq(N, N * 1.1, by = 0.5), objective_continuous(seq(N, N * 1.1, by = 0.5)), type='b', col="black", xlab="n", ylab="Objective", main=paste("Objective vs n to find optimal tickets sold \n(", nc, ") gamma =", gamma, "N =", N, "continuous"))
  abline(h = 0, col = 'blue', lty = 2)
  abline(v = nc, col = 'blue', lty = 2)

  # Returning a named list containing nd, nc, N, p, and gamma
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

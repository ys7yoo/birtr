#' Item Characteristic Curve Calculations
#' @description
#' Computes the logistic deviate L, the exponent of negative L,
#' the denominator, and the value of probability of correct response for each
#' of seven ability levels evenly spaced from -3 to +3 under the one-, two-,
#' or three-parameter logistic item characteristic curve model.
#' @usage
#' icccal(b, a, c)
#' @param b a single number representing the value of item difficulty.
#' @param a a single number representing the value of item discrimination.
#' @param c a single number representing the value of lower asymptote.
#' @keywords icc
#' @export
#' @details
#' While the theoretical range of ability is from negative infinity to positive
#' infinity, practical considerations usually limit the range of values
#' from -3 to +3.
#' Under the one-parameter logistic model, \code{a = 1} and \code{c = 0}.
#' Under the two-parameter logistic model, \code{c = 0}.
#' The parameter \code{c} has a theoretical range from 0 to 1, but in practice
#' values above .35 are not considered acceptable, hence use the range from 0
#' to .35 for \code{c}.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' icccal(1.5, 1.3, .2)
#' icccal(a = 1.3, b = 1.5, c = .2)
#' icccal(1)      # icccal(1, 1, 0)
#' icccal(1, 0.5) # icccal(1, 0.5, 0)

icccal <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  theta <- seq(-3, 3, 1)
  L <- a * (theta - b)
  expnl <- exp(-L)
  opexpnl <- 1 + expnl
  P <- c + (1 - c) / opexpnl
  data.frame(theta, L, expnl, opexpnl, P)
}



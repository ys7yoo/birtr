#' Test Characteristic Curve
#' @description
#' Plots a test characteristic curve from a set of item parameters under
#' the one-, two-, or three-parameter logistic model.
#' @usage
#' tcc(b, a, c)
#' @param b a numeric vector representing the values of item difficulty.
#' @param a a numeric vector representing the values of item discrimination.
#' @param c a numeric vector representing the values of lower asymptote.
#' @keywords tcc
#' @export
#' @details
#' While the theoretical range of ability is from negative infinity to positive
#' infinity, practical considerations usually limit the range of values
#' from -3 to +3.
#' The length of \code{b} should be the same as that of \code{a} and \code{c}.
#' Each parameter \code{c} has a theoretical range from 0 to 1, but in practice
#' values above .35 are not considered acceptable, hence use the range from 0
#' to .35 for each \code{c}.
#' Under the one-parameter logistic model, \code{a = rep(1, length(b))} and
#' \code{c = rep(0, length(b))}.
#' Under the two-parameter logistic model, \code{c = rep(0, length(b))}.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' b <- c(-2.0, -1.0, 0.0, 1.0, 2.0)
#' a <- c(0.5, 0.75, 1.0, 0.75, 0.5)
#' c <- c(.2, .2, .2, .2, .2)
#' tcc(b, a, c)
#' tcc(a = a, b = b, c = c)
#' tcc(b)    # tcc(b, a = rep(1, length(b)), c = rep(0, length(b)))
#' tcc(b, a) # tcc(b, a, c = rep(0, length(b)))

tcc <- function(b, a, c) {
  J <- length(b)
  if (missing(c)) c <- rep(0, J)
  if (missing(a)) a <- rep(1, J)
  theta <- seq(-3, 3, .1)
  ts <- rep(0, length(theta))
  for (j in 1:J) {
    P <- c[j] + (1 - c[j]) / (1 + exp(-a[j]*(theta-b[j])))
    ts <- ts + P
  }
  plot(theta, ts, type="l", xlim=c(-3,3), ylim=c(0,J),
       xlab="Ability", ylab="True Score",
       main="Test Characteristic Curve")
}

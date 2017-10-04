#' Test Information Function
#' @description
#' Plots a test information function from a set of item parameters under
#' the one-, two-, or three-parameter logistic model.
#' @usage
#' tif(b, a, c)
#' @param b a numeric vector representing the values of item difficulty.
#' @param a a numeric vector representing the values of item discrimination.
#' @param c a numeric vector representing the values of lower asymptote.
#' @keywords tif
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
#' In case \code{b} to be a single number, then the plot contains the item
#' informaiton function.
#' Note that the maximum of the information value on the vertical axis of
#' the graph is arbitrarily set to 10.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' b <- c(-1.0, -0.5, 0.0, 0.5, 1.0)
#' a <- c(2.0, 1.5, 1.5, 1.5, 2.0)
#' c <- c(.2, .2, .2, .2, .2)
#' tif(b, a, c)
#' tif(a = a, b = b, c = c)
#' tif(b)    # tif(b, a = rep(1, length(b)), c = rep(0, length(b)))
#' tif(b, a) # tif(b, a, c = rep(0, length(b)))

tif <- function(b, a, c) {
  J <- length(b)
  if (missing(c)) c <- rep(0, J)
  if (missing(a)) a <- rep(1, J)
  theta <- seq(-3, 3, 0.1)
  ii <- matrix(rep(0, length(theta)*J), nrow=length(theta))
  i <- rep(0, length(theta))
  for (j in 1:J) {
    Pstar <- 1 / (1 + exp(-a[j] * (theta - b[j])))
    P <- c[j] + (1 - c[j]) * Pstar
    ii[,j] <- a[j]**2 * P * (1.0 - P) * (Pstar / P)**2
    i <- i + ii[,j]
  }
  plot(theta, i, xlim=c(-3,3), ylim=c(0,10), type="l",
       xlab="Ability", ylab="Information",
       main="Test Information Function")
}


#' Item Characteristic Curve
#' @description
#' Plots an item characteristic curve under the one-, two-, or three-parameter
#' logistic model.
#' @usage
#' icc(b, a, c)
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
#' The vertical dotted line corresponds to the value of the item difficulty
#' parameter.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' icc(1.5, 1.3, .2)
#' icc(a = 1.3, b = 1.5, c = .2)
#' icc(1)      # icc(1, 1, 0)
#' icc(1, 0.5) # icc(1, 0.5, 0)

icc <- function(b, a, c) {
  if (missing(c)) c <- 0
  if (missing(a)) a <- 1
  par(lab=c(7,3,3))
  theta <- seq(-3, 3, .1)
  P <- c + (1 - c) / (1 + exp(-a * (theta - b)))
  plot(theta, P, type="l", xlim=c(-3,3), ylim=c(0,1),
       xlab="Ability", ylab="Probability of Correct Response")
  thetai <- b
  pthetai <- c + (1 - c) / (1 + exp(-a * (thetai - b)))
  vliney <- seq(0, pthetai, .01)
  vlinex <- b + vliney * 0
  lines(vlinex, vliney, lty=2)
}


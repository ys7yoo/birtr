#' Ability Estimation
#' @description
#' Estimates the ability parameter and obtains the standard error of the
#' estimate given the item characteristic curve model, the response vector,
#' and the set of known item parameters under the one-, two-, or
#' three-parameter logistic model.
#' @usage
#' ability(mdl, u, b, a, c)
#' @param mdl 1, 2, or 3 representing the number of the model parameters.
#' @param u a numeric vector of 0s and 1s representing the responses to items.
#' @param b a numeric vector representing the values of item difficulty.
#' @param a a numeric vector representing the values of item discrimination.
#' @param c a numeric vector representing the values of lower asymptote.
#' @keywords groupinv
#' @import utils
#' @export
#' @details
#' With the number of item characteristic curve model parameters \code{mdl},
#' the response vector \code{u}, and the set of item parameters \code{b},
#' \code{a}, and \code{c}, the ability parameter is estimated and reported
#' as \code{th} by the maximum likelilhood procedure.
#' The estimated standard error \code{se} is also obtained and reported.
#' The length of \code{u} should be the same as that of \code{b}, \code{a},
#' and \code{c}.
#' Each parameter \code{c} has a theoretical range from 0 to 1, but in
#' practice values above .35 are not considered acceptable, hence use the
#' range from 0 to .35 for each \code{c}.
#' Under the one-parameter logisric model, \code{a = rep(1, length(b))} and
#' \code{c = rep(0, length(b))}.
#' Under the two-parameter logistic model, \code{c = rep(0, lenght(b))}.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' u <- c(1, 0, 1)
#' b <- c(-1.0, 0.0, 1.0)
#' a <- c(1.0, 1.2, 0.8)
#' ability(2, u, b, a) # ability(2, u, b, a, c = rep(0, length(b)))
#' theta.se <- ability(2, u, b, a)
#' theta.se

ability <- function(mdl, u, b, a, c) {
  J <- length(b)
  if (mdl == 1 | mdl == 2 | missing(c)) {
    c <- rep(0, J)
  }
  if (mdl == 1 | missing(a)) { a <- rep(1, J) }
  x <- sum(u)
  if (x == 0) {
    th <- -log(2 * J)
  }
  if (x == J) {
    th <- log(2 * J)
  }
  if (x == 0 | x == J) {
    sumdem <- 0.0
    for (j in 1:J) {
      pstar <- 1 / (1 + exp(-a[j] * (th - b[j])))
      phat <- c[j] + (1.0 - c[j]) * pstar
      sumdem <- sumdem - a[j]**2 * phat * (1.0 - phat) *
        (pstar / phat)**2
    }
    se <- 1 / sqrt(-sumdem)
  }
  if (x != 0 & x != J) {
    th <- log(x / (J - x))
    S <- 10
    ccrit <- 0.001
    for (s in 1:S) {
      sumnum <- 0.0
      sumdem <- 0.0
      for (j in 1:J) {
        pstar <- 1 / (1 + exp(-a[j] * (th - b[j])))
        phat <- c[j] + (1.0 - c[j]) * pstar
        sumnum <- sumnum + a[j] * (u[j] - phat) *
          (pstar / phat)
        sumdem <- sumdem - a[j]**2 * phat * (1.0 - phat) *
          (pstar / phat)**2
      }
      delta <- sumnum / sumdem
      th <- th - delta
      if (abs(delta) < ccrit | s == S) {
        se <- 1 / sqrt(-sumdem)
        break
      }
    }
  }
  cat(paste("th=", th, "\n")); flush.console()
  cat(paste("se=", se, "\n")); flush.console()
  thse <- c(th, se)
  return(thse)
}



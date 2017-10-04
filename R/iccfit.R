#' Item Characteristic Curve Fitting
#' @description
#' Plots the item characteristic curve and the simulated observed proportions
#' of correct response from the one-, two-, or three-parameter logistic model.
#' @usage
#' iccfit(mdl)
#' @param mdl 1, 2, or 3 representing the number of the model parameters.
#' @keywords iccfit
#' @export
#' @details
#' While the theoretical range of ability is from negative infinity to positive
#' infinity, practical considerations usually limit the range of values
#' from -3 to +3.
#' With the number of item characteristic curve model parameters \code{mdl}
#' the item parameters are randomly sampled from the uniform distributions; for
#' example, under the three-parameter logistic model, \code{b} from the -3 to
#' 3 range, \code{a} from the 0.2 to 2.8 range, and \code{c} from the 0 to
#' .35 range.
#' Each of the 33 ability levels from the -3 to +3 range with .1875 interval,
#' the observed proportion of correct response is generated from the binomial
#' distribution for sample size of 21.
#' The chi-square goodness-of-fit index is obtained and reported with the
#' set of item parameters.
#' @references
#' Baker, F. B., & Kim, S.-H. (2017).
#' \emph{The basics of item response theory using R.}
#' New York, NY: Springer.
#' ISBN-13: 978-3-319-54204-1
#' @examples
#' iccfit(1)
#' iccfit(2)
#' iccfit(3)

iccfit <- function(mdl) {
  theta <- seq(-3, 3, .1875)
  f <- rep(21, length(theta))
  wb <- round(runif(1,-3,3), 2)
  wa <- round(runif(1,0.2,2.8), 2)
  wc <- round(runif(1,0,.35), 2)
  if (mdl == 1 | mdl == 2) { wc <- 0 }
  if (mdl == 1) { wa <- 1 }
  for (g in 1:length(theta)) {
    P <- wc + (1 - wc) / (1 + exp(-wa * (theta - wb)))
  }
  p <- rbinom(length(theta), f, P) / f
  par(lab=c(7,5,3))
  plot(theta, p, xlim=c(-3,3), ylim=c(0,1),
       xlab="Ability", ylab="Probability of Correct Response")
  cs <- 0
  for (g in 1:length(theta)) {
    v <- f[g] * (p[g] - P[g])^2 / (P[g] - P[g]^2)
    cs <- cs + v
  }
  cs <- round(cs, 2)
  if (mdl == 1) {
    maintext <- paste("Chi-square=", cs, "\n", "b=", wb)
  }
  if (mdl == 2) {
    maintext <- paste("Chi-square=",cs,"\n","a=",wa,"b=",wb)
  }
  if (mdl == 3) {
    maintext <- paste("Chi-square=", cs, "\n",
                      "a=", wa, "b=", wb, "c=", wc)
  }
  par(new="T")
  plot(theta, P, xlim=c(-3,3), ylim=c(0,1), type="l",
       xlab="", ylab="", main=maintext)
}

